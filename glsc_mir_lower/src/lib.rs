use glsc_hir::{self as hir, MemberAccessKind};
use glsc_mir::{self as mir};
use lang_c::ast::{self, Integer, IntegerSuffix};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Scope {
    pub typedefs: HashMap<mir::Identifier, mir::Ty>,
    // Track the variable name and Ty, dont't care about the value for now
    pub variables: HashMap<mir::Identifier, mir::Ty>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            typedefs: HashMap::new(),
            variables: HashMap::new(),
        }
    }
}
#[derive(Debug)]
pub struct MirLower {
    pub scope_stack: Vec<Scope>,
    /// technically maybe this should be called external declaration
    pub global_declarations: Vec<mir::ExternalDeclaration>,
    internal_label: u64,
}

impl MirLower {
    pub fn new() -> Self {
        Self {
            // Global scope
            scope_stack: vec![Scope::new()],
            internal_label: 0,
            global_declarations: vec![],
        }
    }
    pub fn get_current_scope(&mut self) -> &mut Scope {
        self.scope_stack.last_mut().unwrap()
    }
    pub fn resolve_var_ty(&mut self, identifier: &mir::Identifier) -> Option<&glsc_mir::Ty> {
        for scope in &*self.scope_stack {
            if let Some(var) = scope.variables.get(identifier) {
                return Some(var);
            }
        }
        return None;
    }
    pub fn resolve_typedef(&mut self, identifier: &mir::Identifier) -> Option<&glsc_mir::Ty> {
        for scope in &*self.scope_stack {
            if let Some(var) = scope.typedefs.get(identifier) {
                return Some(var);
            }
        }
        return None;
    }
    pub fn push_scope(&mut self) {
        self.scope_stack.push(Scope::new());
    }
    pub fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }
}

impl MirLower {
    pub fn lower_translation_unit(&mut self, unit: &hir::TranslationUnit) {
        for declaration in &unit.declarations {
            self.lower_external_declaration(declaration);
        }
    }
    pub fn lower_external_declaration(&mut self, external_declaration: &hir::ExternalDeclaration) {
        match external_declaration {
            hir::ExternalDeclaration::FunctionDefinition(function_definition) => {
                let function_definition = self.lower_function_definition(function_definition);
                self.global_declarations
                    .push(mir::ExternalDeclaration::FunctionDefinition(
                        function_definition,
                    ));
            }
            hir::ExternalDeclaration::Declaration(declaration) => {
                let mir_declaration = self.lower_declaration(declaration);
                self.declare_in_current_scope_if_typedef(declaration);

                self.global_declarations
                    .push(mir::ExternalDeclaration::Declaration(mir_declaration));
            }
        }
    }
    /// Currently this only handles typedefs
    pub fn declare_in_current_scope_if_typedef(&mut self, declaration: &hir::Declaration) {
        let mir_declaration = self.lower_declaration(declaration);

        if let Some(ast::StorageClassSpecifier::Typedef) = declaration.ty.storage_class {
            self.get_current_scope()
                .typedefs
                .insert(mir_declaration.name, mir_declaration.ty);
        } else {
            self.get_current_scope()
                .variables
                .insert(mir_declaration.name, mir_declaration.ty);
        }
    }
    pub fn lower_declaration(&mut self, declaration: &hir::Declaration) -> mir::Declaration {
        let mir_ty = self.lower_ty(&declaration.ty.clone());
        let body = declaration
            .init
            .as_ref()
            .map(|expression| self.lower_expression(expression));

        mir::Declaration {
            ty: mir_ty,
            init: body,
            name: declaration.name.clone().into(),
        }
    }
    pub fn lower_function_definition(
        &mut self,
        function_definition: &hir::FunctionDefinition,
    ) -> mir::FunctionDefinition {
        let name = function_definition.name.clone().into();
        let return_type = self.lower_ty(&function_definition.return_type);

        let mut parameters = vec![];
        for parameter in &function_definition.parameters {
            parameters.push(mir::FunctionParameter {
                name: parameter.name.as_ref().map(|n| n.into()),
                ty: self.lower_ty(&parameter.ty),
            });
        }
        let body = self
            .lower_statement(&function_definition.body)
            .expect("Function definition with empty body?");
        mir::FunctionDefinition {
            return_type,
            name,
            parameters,
            body,
        }
    }
    pub fn lower_ty(&mut self, ty: &hir::Ty) -> mir::Ty {
        match &ty.data_type {
            hir::DataType::None => mir::Ty::None,
            hir::DataType::Void => mir::Ty::Void,
            hir::DataType::Short => mir::Ty::Short,
            hir::DataType::Int => mir::Ty::Int,
            hir::DataType::Float => mir::Ty::Float,
            hir::DataType::Double => mir::Ty::Double,
            hir::DataType::TS18661Float(ts18661_float_type) => todo!(),
            hir::DataType::Char => mir::Ty::Char,
            hir::DataType::Long => mir::Ty::Long,
            hir::DataType::LongLong => mir::Ty::LongLong,
            hir::DataType::TypedefName(identifier) => {
                let identifier = identifier.into();
                self.resolve_typedef(&identifier)
                    .expect("Failed to find typedef. ")
                    .clone()
            }
            hir::DataType::Pointer(ty) => mir::Ty::Pointer(Box::new(self.lower_ty(ty))),
            hir::DataType::Function {
                return_type,
                parameters,
            } => mir::Ty::Function {
                return_type: Box::new(self.lower_ty(&return_type)),
                parameters: parameters
                    .iter()
                    .map(|param| mir::FunctionParameter {
                        name: param.name.as_ref().map(|ident| ident.clone().into()),
                        ty: self.lower_ty(&param.ty),
                    })
                    .collect::<Vec<_>>(),
            },
            // TODO: add padding to each field
            hir::DataType::Struct { name, fields } => {
                let fields = fields
                    .iter()
                    .map(|field| mir::StructField {
                        name: field.name.clone().into(),
                        ty: self.lower_ty(&field.ty),
                    })
                    .collect();
                mir::Ty::Struct {
                    name: name.as_ref().map(|n| n.into()),
                    fields,
                }
            }
        }
    }
    pub fn lower_statement(&mut self, statement: &hir::Statement) -> Option<mir::Statement> {
        match statement {
            hir::Statement::Compound(statements) => {
                self.push_scope();
                let lowered_statements = mir::Statement::Compound(
                    statements
                        .iter()
                        .map(|statement| self.lower_statement(statement))
                        .filter(|statement| statement.is_some())
                        .map(|statement| statement.unwrap())
                        .collect::<Vec<_>>(),
                );
                self.pop_scope();
                Some(lowered_statements)
            }
            hir::Statement::Return(expression) => Some(mir::Statement::Return(
                expression
                    .as_ref()
                    .map(|expression| self.lower_expression(expression)),
            )),
            hir::Statement::Declaration(declaration) => {
                self.declare_in_current_scope_if_typedef(declaration);
                Some(glsc_mir::Statement::Declaration(
                    self.lower_declaration(declaration),
                ))
            }
            hir::Statement::Expression(None) => None,
            hir::Statement::Expression(Some(expression)) => Some(mir::Statement::Expression(Some(
                self.lower_expression(expression),
            ))),
            hir::Statement::If(expression, then, elze) => Some(mir::Statement::If(
                self.lower_expression(expression),
                Box::new(self.lower_statement(then).unwrap()),
                Box::new(
                    elze.clone()
                        .map(|stmt| self.lower_statement(&stmt).unwrap()),
                ),
            )),
            hir::Statement::For(for_initializer, condition, update, body) => {
                let mut stmts = vec![];
                let init = match for_initializer {
                    hir::ForInitializer::Empty => mir::Statement::Empty,
                    hir::ForInitializer::Declaration(declaration) => {
                        mir::Statement::Declaration(self.lower_declaration(declaration))
                    }
                    hir::ForInitializer::Expression(expression) => {
                        mir::Statement::Expression(Some(self.lower_expression(expression)))
                    }
                };

                stmts.push(init);

                let loop_start = self.next_internal_label();
                let loop_end = self.next_internal_label();

                let loop_entry = mir::Statement::If(
                    self.lower_expression(&condition.as_ref().unwrap()),
                    Box::new(mir::Statement::Compound(vec![
                        self.lower_statement(&body).unwrap(),
                        mir::Statement::Expression(Some(
                            self.lower_expression(&update.as_ref().unwrap()),
                        )),
                        mir::Statement::GotoInternal(loop_start),
                    ])),
                    Box::new(Some(mir::Statement::GotoInternal(loop_end))),
                );

                stmts.push(mir::Statement::LabeledStatement(
                    glsc_mir::Label::Internal(loop_start),
                    Box::new(loop_entry),
                ));
                // Empty label technically contains everything after it
                stmts.push(mir::Statement::LabeledStatement(
                    glsc_mir::Label::Internal(loop_end),
                    Box::new(mir::Statement::Empty),
                ));

                Some(mir::Statement::Compound(stmts))
            }
            hir::Statement::LabeledStatement(label, statement) => todo!(),
            hir::Statement::Goto(identifier) => {
                Some(glsc_mir::Statement::GotoLabel(identifier.into()))
            }
        }
    }
    pub fn next_internal_label(&mut self) -> u64 {
        let internal_label = self.internal_label;
        self.internal_label += 1;
        return internal_label;
    }
    pub fn lower_expression(&mut self, expression: &hir::Expression) -> mir::Expression {
        match expression {
            hir::Expression::Identifier(identifier) => {
                mir::Expression::Identifier(identifier.into())
            }
            hir::Expression::BinOp(lhs, binary_operator, rhs) => match binary_operator {
                _ => mir::Expression::BinOp(
                    Box::new(self.lower_expression(&lhs)),
                    binary_operator.clone(),
                    Box::new(self.lower_expression(&rhs)),
                ),
            },
            hir::Expression::UnaryOp(unary_operator, expression) => mir::Expression::UnaryOp(
                unary_operator.clone(),
                Box::new(self.lower_expression(&expression)),
            ),
            hir::Expression::Constant(constant) => mir::Expression::Constant(constant.clone()),
            hir::Expression::Member(expression, identifier, member_access_kind) => {
                assert!(matches!(member_access_kind, MemberAccessKind::Direct));
                let mir_expression = self.lower_expression(expression);
                let expression_ty = self.get_expression_type(&mir_expression);

                let (offset, field_ty) = self
                    .get_struct_field_offset(&expression_ty, &identifier.into())
                    .expect(&format!("couldnt find struct field {:?} for struct type {:?}", identifier, expression_ty));

                let address =
                    mir::Expression::UnaryOp(ast::UnaryOperator::Address, Box::new(mir_expression));
                let plus_offset = mir::Expression::BinOp(
                    Box::new(address),
                    ast::BinaryOperator::Plus,
                    Box::new(mir::Expression::Constant(ast::Constant::Integer(Integer {
                        base: ast::IntegerBase::Decimal,
                        number: format!("{}", offset).into(),
                        suffix: IntegerSuffix {
                            imaginary: false,
                            size: ast::IntegerSize::Long,
                            unsigned: false,
                        },
                    }))),
                );
                let deref = mir::Expression::UnaryOp(
                    ast::UnaryOperator::Indirection,
                    Box::new(mir::Expression::Cast(Box::new(plus_offset), mir::Ty::Pointer(Box::new(field_ty)))),
                );
                deref
            }
            hir::Expression::Call(expression, arguments) => {
                let expression = self.lower_expression(expression);
                let args = arguments
                    .iter()
                    .map(|arg| self.lower_expression(&arg))
                    .collect::<Vec<_>>();

                mir::Expression::Call(Box::new(expression), args)
            }
            hir::Expression::StringLiteral(string) => {
                mir::Expression::StringLiteral(string.clone())
            }
        }
    }
    fn get_struct_field_offset(
        &mut self,
        struct_type: &mir::Ty,
        field_name: &mir::Identifier,
    ) -> Option<(u32, mir::Ty)> {
        match struct_type {
            glsc_mir::Ty::Struct {
                name: _name,
                fields,
            } => {
                let mut offset = 0;

                for field in fields {
                    if field.name == *field_name {
                        return Some((offset, field.ty.clone()));
                    } else {
                        offset += field.ty.get_size_on_stack();
                        continue;
                    }
                }
                return None;
            }
            _ => unreachable!(),
        }
    }
    fn get_expression_type(&mut self, expression: &mir::Expression) -> mir::Ty {
        match expression {
            glsc_mir::Expression::Identifier(identifier) => {
                let ty = self.resolve_var_ty(identifier).unwrap();
                ty.clone()
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
                ast::UnaryOperator::Address => {
                    mir::Ty::Pointer(Box::new(self.get_expression_type(expression)))
                }
                ast::UnaryOperator::Indirection => {
                    let full_ty = self.get_expression_type(expression);
                    match full_ty {
                        glsc_mir::Ty::Pointer(ty) => *ty,
                        _ => panic!("Indirection on non pointer ty {:?}", full_ty),
                    }
                }
                _ => self.get_expression_type(&expression),
            },
            glsc_mir::Expression::Constant(constant) => mir::Ty::Int,
            glsc_mir::Expression::Call(expression, vec) => unimplemented!(),
            glsc_mir::Expression::StringLiteral(_) => mir::Ty::Pointer(Box::new(mir::Ty::Char)),
            glsc_mir::Expression::Cast(expression, ty) => {
                match ty {
                    glsc_mir::Ty::Pointer(pointer_ty) => {
                        ty.clone()
                    },
                    _ => todo!("cast between {:?} for expr {:?}", ty, expression)
                }
            },
            
        }
    }
}
