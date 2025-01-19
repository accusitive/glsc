use std::{collections::HashMap, ffi::CString};

use ahash::RandomState;
use cranelift::codegen::Context;
use cranelift::prelude::*;
use cranelift_codegen::{
    ir::{function, ArgumentExtension, ArgumentPurpose, Inst, StackSlot, UserFuncName},
    verify_function,
};
use cranelift_module::{DataDescription, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use glsc_mir::{self as mir, FunctionParameter, Identifier};
use lang_c::ast::{BinaryOperator, UnaryOperator};
use target_lexicon::BinaryFormat;

/// Pointer width
const ADDR_TYPE: Type = types::I64;

pub struct Backend {
    pub module: ObjectModule,
    pub ctx: Context,

    scope_stack: Vec<Scope>,
}
#[derive(Debug, Clone)]
struct Variable {
    kind: VariableKind,
    mir_ty: mir::Ty,
}
#[derive(Debug, Clone)]
enum VariableKind {
    Slot(StackSlot),
    Value(Value),
}
#[derive(Debug, Default)]
struct Scope {
    variables: HashMap<mir::Identifier, Variable>,
    function_designations: HashMap<mir::Identifier, (mir::FunctionDesignation, FuncId)>,
}
struct FunctionTranslator<'a, M: Module> {
    builder: FunctionBuilder<'a>,
    module: &'a mut M,
    scope_stack: &'a mut Vec<Scope>,
}

type CliffTy = cranelift_codegen::ir::Type;

impl Backend {
    pub fn new() -> Self {
        let mut build_flags = cranelift::codegen::settings::builder();
        build_flags.set("opt_level", "speed_and_size").unwrap();
        build_flags.enable("enable_alias_analysis").unwrap();

        let flags = settings::Flags::new(build_flags);

        let mut target = target_lexicon::triple!("x86_64");
        target.binary_format = BinaryFormat::Elf;

        let isa_builder = cranelift_codegen::isa::lookup(target).unwrap();
        let isa = isa_builder.finish(flags).unwrap();

        let builder =
            ObjectBuilder::new(isa, "program", cranelift_module::default_libcall_names()).unwrap();

        let module = ObjectModule::new(builder);
        let ctx = module.make_context();
        Self {
            ctx,
            module,
            scope_stack: vec![],
        }
    }
    pub fn compile(mut self, mir: &glsc_mir_lower::MirLower) -> Vec<u8> {
        self.scope_stack.push(Scope::default());
        for declaration in &mir.global_declarations {
            match declaration {
                glsc_mir::ExternalDeclaration::FunctionDefinition(function_definition) => {
                    self.define_function(&function_definition);
                    // println!("{}", self.ctx.func);
                }
                glsc_mir::ExternalDeclaration::Declaration(declaration) => {
                    if let mir::Ty::Function {
                        return_type,
                        parameters,
                    } = &declaration.ty
                    {
                        let definition = mir::FunctionDesignation {
                            name: declaration.name.clone(),
                            parameters: parameters.clone(),
                            return_type: *return_type.clone(),
                        };
                        self.designate_external_function(&definition);
                    } else {
                        todo!()
                    }
                }
            }
        }
        // Not sure if this is necessary since nothing will be processing after this point anyway
        self.scope_stack.pop();
        self.module.finish().emit().unwrap()
    }
    pub fn designate_external_function(&mut self, function_designation: &mir::FunctionDesignation) {
        let sig = self.function_signature(
            &function_designation.parameters,
            &function_designation.return_type,
        );
        self.ctx.func.signature = sig.clone();
        println!("{}", self.ctx.func);

        let func_id = self
            .module
            .declare_function(&function_designation.name.0, Linkage::Import, &sig)
            .unwrap();
        self.scope_stack
            .last_mut()
            .unwrap()
            .function_designations
            .insert(
                function_designation.name.clone(),
                (function_designation.clone(), func_id),
            );
    }
    pub fn define_function(&mut self, function_definition: &mir::FunctionDefinition) {
        let sig = self.function_signature(
            &function_definition.parameters,
            &function_definition.return_type,
        );
        let func_id = self
            .module
            .declare_function(&function_definition.name.0, Linkage::Export, &sig)
            .unwrap();
        self.ctx.func.signature = sig;
        let mut fbctx = FunctionBuilderContext::new();

        let builder = FunctionBuilder::new(&mut self.ctx.func, &mut fbctx);
        let mut translator = FunctionTranslator {
            builder,
            module: &mut self.module,
            scope_stack: &mut self.scope_stack,
        };

        translator.compile_function_body(function_definition);
        println!("{}", self.ctx.func);

        self.module.define_function(func_id, &mut self.ctx).unwrap();

        self.verify_function();
        // self.ctx.clear();
    }
    pub fn collect_all_function_designations(&self) -> Vec<&(mir::FunctionDesignation, FuncId)> {
        let mut z = vec![];
        for scope in &*self.scope_stack {
            for (_, function_designation) in scope.function_designations.iter() {
                z.push(function_designation)
            }
        }

        z
    }
    pub fn function_signature(
        &self,
        parameters: &[FunctionParameter],
        return_type: &mir::Ty,
    ) -> Signature {
        let mut sig = self.module.make_signature();

        for parameter in parameters {
            sig.params.push(AbiParam {
                value_type: Self::cliff_ty(&parameter.ty),
                purpose: ArgumentPurpose::Normal,
                extension: ArgumentExtension::None,
            });
        }
        if !matches!(return_type, mir::Ty::Void) {
            sig.returns.push(AbiParam {
                value_type: Self::cliff_ty(&return_type),
                purpose: ArgumentPurpose::Normal,
                extension: ArgumentExtension::None,
            });
        }
        sig
    }
    pub fn cliff_ty(ty: &mir::Ty) -> CliffTy {
        match ty {
            glsc_mir::Ty::Void => todo!(),
            glsc_mir::Ty::Short => types::I16,
            glsc_mir::Ty::Int => types::I32,
            glsc_mir::Ty::Long => types::I64,
            glsc_mir::Ty::Float => types::F32,
            glsc_mir::Ty::Double => types::F64,
            glsc_mir::Ty::TS18661Float(ts18661_float_type) => todo!(),
            glsc_mir::Ty::Char => types::I8,
            glsc_mir::Ty::LongLong => types::I128,
            glsc_mir::Ty::Pointer(ty) => ADDR_TYPE,
            // These will all be abstracted away - hopefully
            glsc_mir::Ty::Struct { .. }
            | glsc_mir::Ty::TypedefName(_)
            | glsc_mir::Ty::None
            | glsc_mir::Ty::Function { .. } => {
                unreachable!()
            }
        }
    }
    pub fn verify_function(&self) {
        let mut build_flags = settings::builder();
        build_flags.set("opt_level", "speed_and_size").unwrap();
        let flags = settings::Flags::new(build_flags);

        verify_function(&self.ctx.func, &flags).unwrap();
    }
}
impl<'a, M: Module> FunctionTranslator<'a, M> {
    pub fn compile_function_body(&mut self, function_definition: &mir::FunctionDefinition) {
        self.scope_stack.push(Scope::default());

        let entry = self.builder.create_block();
        self.builder.switch_to_block(entry);
        self.builder.seal_block(entry);
        self.builder.append_block_params_for_function_params(entry);
        for (block_param, parameter) in self
            .builder
            .block_params(entry)
            .iter()
            .zip(function_definition.parameters.iter())
        {
            self.scope_stack.last_mut().unwrap().variables.insert(
                parameter.name.as_ref().unwrap().clone(),
                Variable {
                    kind: VariableKind::Value(*block_param),
                    mir_ty: parameter.ty.clone(),
                },
            );
        }
        self.compile_statement(&function_definition.body);
        self.scope_stack.pop();
    }
    pub fn compile_statement(&mut self, statement: &mir::Statement) {
        match statement {
            glsc_mir::Statement::Compound(statements) => {
                for statement in statements {
                    self.compile_statement(statement);
                }
            }
            glsc_mir::Statement::Return(expression) => {
                if let Some(expression) = expression {
                    let compiled_expression = self.compile_expression(expression);
                    self.builder.ins().return_(&[compiled_expression]);
                }
            }
            glsc_mir::Statement::Declaration(declaration) => {
                let slot = self.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: Self::get_size_on_stack(&declaration.ty),
                    align_shift: 4,
                });
                self.scope_stack.last_mut().unwrap().variables.insert(
                    declaration.name.clone(),
                    Variable {
                        kind: VariableKind::Slot(slot),
                        mir_ty: declaration.ty.clone(),
                    },
                );
                if let Some(init) = &declaration.init {
                    let compiled_init = self.compile_expression(init);

                    if self.get_expression_type(init) != declaration.ty {
                        panic!("Declaration type not the same as expression type");
                    }
                    let slot_addr = self.builder.ins().stack_addr(ADDR_TYPE, slot, 0);

                    self.builder
                        .ins()
                        .store(MemFlags::new(), compiled_init, slot_addr, 0);
                }
            }
            glsc_mir::Statement::Expression(None) => {}
            glsc_mir::Statement::Expression(Some(expression)) => {
                self.compile_expression(expression);
            }
            glsc_mir::Statement::LabeledStatement(label, statement) => todo!(),
            glsc_mir::Statement::GotoLabel(identifier) => todo!(),
            glsc_mir::Statement::GotoInternal(_) => todo!(),
            glsc_mir::Statement::Empty => todo!(),
            glsc_mir::Statement::If(expression, statement, statement1) => todo!(),
        }
    }
    fn get_size_on_stack(ty: &mir::Ty) -> u32 {
        match ty {
            glsc_mir::Ty::Short => 2,
            glsc_mir::Ty::Int => 4,
            glsc_mir::Ty::Float => 4,
            glsc_mir::Ty::Double => 8,
            glsc_mir::Ty::Char => 1,
            glsc_mir::Ty::Long => 8, // Assuming 64-bit system
            glsc_mir::Ty::LongLong => 8,
            glsc_mir::Ty::Pointer(_) => 8,
            _ => unimplemented!("type {:?} is not implemented", ty),
        }
    }
    pub fn compile_expression(&mut self, expression: &mir::Expression) -> Value {
        match expression {
            glsc_mir::Expression::Identifier(identifier) => {
                let var = self
                    .find_variable_in_scope_stack(identifier)
                    .unwrap()
                    .clone();

                match var.kind {
                    VariableKind::Slot(stack_slot) => {
                        self.builder
                            .ins()
                            .stack_load(Backend::cliff_ty(&var.mir_ty), stack_slot, 0)
                    }
                    VariableKind::Value(value) => value,
                }
            }
            glsc_mir::Expression::BinOp(lhs, binary_operator, rhs) => {
                self.compile_binop(lhs, binary_operator, rhs)
            }
            glsc_mir::Expression::UnaryOp(unary_operator, expression) => {
                self.compile_unop(unary_operator, &expression)
            }
            glsc_mir::Expression::Constant(constant) => self.compile_constant(constant),
            glsc_mir::Expression::Call(expression, args) => self.compile_call(expression, args),
            glsc_mir::Expression::StringLiteral(string) => {
                let hash_builder = RandomState::with_seed(0);

                let id = self
                    .module
                    .declare_data(
                        &hash_builder.hash_one(string).to_string(),
                        Linkage::Export,
                        true,
                        false,
                    )
                    .unwrap();
                // TODO: fix strings
                let mut data = DataDescription::new();
                data.define(
                    CString::new(string.clone())
                        .unwrap()
                        .as_bytes_with_nul()
                        .into(),
                );
                // TODO: will this support utf-8 etc?
                self.module.define_data(id, &data).unwrap();
                data.clear();
                let gv = self.module.declare_data_in_func(id, self.builder.func);

                self.builder.ins().global_value(ADDR_TYPE, gv)
            }
        }
    }
    pub fn compile_call(
        &mut self,
        expression: &mir::Expression,
        args: &[mir::Expression],
    ) -> Value {
        match expression {
            glsc_mir::Expression::Identifier(identifier) => {
                let (designation, func_id) = self
                    .find_function_designation_in_scope_stack(identifier)
                    .unwrap();
                let func_ref = self
                    .module
                    .declare_func_in_func(*func_id, &mut self.builder.func);
                let args = args
                    .iter()
                    .map(|arg| self.compile_expression(arg))
                    .collect::<Vec<_>>();
                let call_inst = self.builder.ins().call(func_ref, &args);
                let call_result = self.builder.inst_results(call_inst)[0];

                call_result
            }
            _ => panic!("indirect calls not supported yet"),
        }
        // let args = args
        //     .iter()
        //     .map(|arg| self.compile_expression(arg))
        //     .collect::<Vec<_>>();
        // let sig = self.module.make_signature();

        // let indirect_call =
        //     self.builder
        //         .ins()
        //         .call_indirect(sig, self.compile_expression(expression), &args);
        // let results = self.builder.inst_results(indirect_call);

        // results[0]
        // unimplemented!()
    }
    fn get_expression_type(&mut self, expression: &mir::Expression) -> mir::Ty {
        match expression {
            glsc_mir::Expression::Identifier(identifier) => {
                let var = self.find_variable_in_scope_stack(identifier).unwrap();
                var.mir_ty.clone()
            }
            glsc_mir::Expression::BinOp(expression, binary_operator, expression1) => {
                let lty = self.get_expression_type(expression);
                let rty = self.get_expression_type(expression);

                if lty != rty {
                    panic!("add between different types is not supported");
                } else {
                    lty.clone()
                }
            }
            glsc_mir::Expression::UnaryOp(unary_operator, expression) => match unary_operator {
                UnaryOperator::Address => {
                    mir::Ty::Pointer(Box::new(self.get_expression_type(expression)))
                }
                UnaryOperator::Indirection => {
                    let full_ty = self.get_expression_type(expression);
                    match full_ty {
                        glsc_mir::Ty::Pointer(ty) => *ty,
                        _ => panic!("Indirection on non pointer ty"),
                    }
                }
                _ => self.get_expression_type(&expression),
            },
            glsc_mir::Expression::Constant(constant) => mir::Ty::Int,
            glsc_mir::Expression::Call(expression, vec) => unimplemented!(),
            glsc_mir::Expression::StringLiteral(_) => mir::Ty::Pointer(Box::new(mir::Ty::Char)),
        }
    }
    fn find_variable_in_scope_stack(&self, i: &Identifier) -> Option<&Variable> {
        for scope in &*self.scope_stack {
            if let Some(var) = scope.variables.get(i) {
                return Some(var);
            }
        }
        return None;
    }
    fn find_function_designation_in_scope_stack(
        &self,
        i: &Identifier,
    ) -> Option<&(mir::FunctionDesignation, FuncId)> {
        for scope in &*self.scope_stack {
            if let Some(var) = scope.function_designations.get(i) {
                return Some(var);
            }
        }
        return None;
    }
    pub fn compile_binop(
        &mut self,
        lhs: &mir::Expression,
        op: &BinaryOperator,
        rhs: &mir::Expression,
    ) -> Value {
        let rhs_value = self.compile_expression(rhs);
        let lhs_value = self.compile_expression(lhs);
        match op {
            BinaryOperator::Plus => self.builder.ins().iadd(lhs_value, rhs_value),
            BinaryOperator::Assign => {
                let loc = self.get_lvalue_location(lhs);
                self.builder.ins().store(MemFlags::new(), rhs_value, loc, 0);
                rhs_value
            }
            _ => unimplemented!(),
        }
    }
    fn get_lvalue_location(&mut self, expression: &mir::Expression) -> Value {
        match expression {
            glsc_mir::Expression::Identifier(identifier) => {
                let v = self.find_variable_in_scope_stack(identifier).unwrap();
                match v.kind {
                    VariableKind::Slot(stack_slot) => {
                        self.builder.ins().stack_addr(ADDR_TYPE, stack_slot, 0)
                    }
                    VariableKind::Value(_) => panic!("trying to assign to not lvalue"),
                }
            }
            glsc_mir::Expression::UnaryOp(UnaryOperator::Indirection, inner_expression)
            | glsc_mir::Expression::UnaryOp(UnaryOperator::Address, inner_expression) => {
                self.compile_expression(inner_expression)
            }
            glsc_mir::Expression::StringLiteral(..)
            | glsc_mir::Expression::Call(..)
            | glsc_mir::Expression::BinOp(..)
            | glsc_mir::Expression::UnaryOp(..)
            | glsc_mir::Expression::Constant(..) => {
                unreachable!("{:?} is not an lvalue expression", expression)
            }
        }
    }
    pub fn compile_unop(
        &mut self,
        operator: &lang_c::ast::UnaryOperator,
        expression: &mir::Expression,
    ) -> Value {
        match operator {
            UnaryOperator::PostIncrement => todo!(),
            UnaryOperator::PostDecrement => todo!(),
            UnaryOperator::PreIncrement => todo!(),
            UnaryOperator::PreDecrement => todo!(),
            UnaryOperator::Address => self.get_lvalue_location(expression),
            UnaryOperator::Indirection => {
                let e = self.compile_expression(expression);
                let expression_type = self.get_expression_type(expression);
                self.builder
                    .ins()
                    .load(Backend::cliff_ty(&expression_type), MemFlags::new(), e, 0)
            }
            UnaryOperator::Plus => todo!(),
            UnaryOperator::Minus => todo!(),
            UnaryOperator::Complement => todo!(),
            UnaryOperator::Negate => todo!(),
        }
    }
    pub fn compile_constant(&mut self, constant: &lang_c::ast::Constant) -> Value {
        match constant {
            lang_c::ast::Constant::Integer(integer) => self.builder.ins().iconst(
                types::I32,
                i64::from_str_radix(&integer.number, Self::base_to_radix(&integer.base)).unwrap(),
            ),
            lang_c::ast::Constant::Float(float) => todo!(),
            lang_c::ast::Constant::Character(_) => todo!(),
        }
    }
    fn base_to_radix(base: &lang_c::ast::IntegerBase) -> u32 {
        match base {
            lang_c::ast::IntegerBase::Binary => 2,
            lang_c::ast::IntegerBase::Octal => 8,
            lang_c::ast::IntegerBase::Decimal => 10,
            lang_c::ast::IntegerBase::Hexadecimal => 16,
        }
    }
}
