use std::collections::HashMap;

use cranelift::codegen::Context;
use cranelift::prelude::*;
use cranelift_codegen::{
    ir::{function, ArgumentExtension, ArgumentPurpose, Inst, StackSlot, UserFuncName},
    verify_function,
};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use glsc_mir as mir;
use target_lexicon::BinaryFormat;
pub struct Backend {
    pub module: ObjectModule,
    pub ctx: Context,

    scope_stack: Vec<Scope>,
}
#[derive(Debug, Default)]
struct Scope {
    variables: HashMap<mir::Identifier, StackSlot>,
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
        for declaration in &mir.global_declarations {
            match declaration {
                glsc_mir::ExternalDeclaration::FunctionDefinition(function_definition) => {
                    self.define_function(&function_definition);
                    println!("{}", self.ctx.func);
                }
                glsc_mir::ExternalDeclaration::Declaration(declaration) => todo!(),
            }
        }
        self.module.finish().emit().unwrap()
    }
    pub fn define_function(&mut self, function_definition: &mir::FunctionDefinition) {
        let sig = self.function_signature(&function_definition);
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
        self.module.define_function(func_id, &mut self.ctx).unwrap();

        self.verify_function();
        // self.ctx.clear();
    }

    pub fn function_signature(&self, function_definition: &mir::FunctionDefinition) -> Signature {
        let mut sig = self.module.make_signature();

        for parameter in &function_definition.parameters {
            sig.params.push(AbiParam {
                value_type: self.cliff_ty(&parameter.ty),
                purpose: ArgumentPurpose::Normal,
                extension: ArgumentExtension::None,
            });
        }
        if !matches!(function_definition.return_type, mir::Ty::Void) {
            sig.returns.push(AbiParam {
                value_type: self.cliff_ty(&function_definition.return_type),
                purpose: ArgumentPurpose::Normal,
                extension: ArgumentExtension::None,
            });
        }
        sig
    }
    pub fn cliff_ty(&self, ty: &mir::Ty) -> CliffTy {
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
            glsc_mir::Ty::Pointer(ty) => todo!(),
            glsc_mir::Ty::TypedefName(_) | glsc_mir::Ty::None | glsc_mir::Ty::Function { .. } => {
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
                    size: 4,
                    align_shift: 4,
                });
                self.scope_stack
                    .last_mut()
                    .unwrap()
                    .variables
                    .insert(declaration.name.clone(), slot);

                if let Some(init) = &declaration.init {
                    let compiled_init = self.compile_expression(init);
                    let slot_addr = self.builder.ins().stack_addr(types::I32, slot, 0);

                    self.builder
                        .ins()
                        .store(MemFlags::new(), compiled_init, slot_addr, 0);

                }
            }
            glsc_mir::Statement::Expression(None) => {}
            glsc_mir::Statement::Expression(Some(expression)) => {
                let compiled_expression = self.compile_expression(expression);
            }
            glsc_mir::Statement::LabeledStatement(label, statement) => todo!(),
            glsc_mir::Statement::GotoLabel(identifier) => todo!(),
            glsc_mir::Statement::GotoInternal(_) => todo!(),
            glsc_mir::Statement::Empty => todo!(),
            glsc_mir::Statement::If(expression, statement, statement1) => todo!(),
        }
    }
    pub fn compile_expression(&mut self, expression: &mir::Expression) -> Value {
        match expression {
            glsc_mir::Expression::Identifier(identifier) => {
                let var = self.scope_stack.last().unwrap().variables.get(identifier).unwrap();

                self.builder.ins().stack_load(types::I32, *var, 0)
            },
            glsc_mir::Expression::BinOp(expression, binary_operator, expression1) => todo!(),
            glsc_mir::Expression::UnaryOp(unary_operator, expression) => todo!(),
            glsc_mir::Expression::Constant(constant) => self.compile_constant(constant),
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
