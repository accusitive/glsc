use glsc_hir::{self as hir};
use glsc_mir::{self as mir};

use std::collections::HashMap;

#[derive(Debug)]
pub struct Scope {
    pub typedefs: HashMap<mir::Identifier, mir::Ty>,
    pub functions: Vec<mir::Function>,
    pub variables: HashMap<mir::Identifier, (mir::Ty, Option<mir::Expression>)>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            typedefs: HashMap::new(),
            functions: Vec::new(),
            variables: HashMap::new(),
        }
    }
}
#[derive(Debug)]
pub struct MirLower {
    pub scopes: Vec<Scope>,
    internal_label: u64,
}

impl MirLower {
    pub fn new() -> Self {
        Self {
            // Global scope
            scopes: vec![Scope::new()],
            internal_label: 0,
        }
    }
    pub fn get_current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }
    pub fn pop_scope(&mut self) {
        self.scopes.pop();
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
            glsc_hir::ExternalDeclaration::FunctionDefinition(function_definition) => {
                self.lower_function_definition(function_definition)
            }
            glsc_hir::ExternalDeclaration::Declaration(declaration) => {
                self.lower_declaration(declaration)
            }
        }
    }
    pub fn lower_declaration(&mut self, declaration: &hir::Declaration) {
        if declaration.ty.is_typedef() {
            assert!(declaration.init.is_none());
            let mir_ty = self.lower_ty(&declaration.ty.clone());
            self.get_current_scope()
                .typedefs
                .insert(declaration.name.clone().into(), mir_ty);
        } else {
            let mir_ty = self.lower_ty(&declaration.ty.clone());
            let body = declaration
                .init
                .as_ref()
                .map(|expression| self.lower_expression(expression));
            self.get_current_scope()
                .variables
                .insert(declaration.name.clone().into(), (mir_ty, body));
        }
    }
    pub fn lower_declaration2(&mut self, declaration: &hir::Declaration) -> mir::Declaration {
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
    pub fn lower_function_definition(&mut self, function_definition: &hir::FunctionDefinition) {
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
        self.get_current_scope().functions.push(mir::Function {
            return_type,
            name,
            parameters,
            body,
        });
    }
    pub fn lower_ty(&mut self, ty: &hir::Ty) -> mir::Ty {
        match &ty.data_type {
            glsc_hir::DataType::None => mir::Ty::None,
            glsc_hir::DataType::Void => mir::Ty::Void,
            glsc_hir::DataType::Short => mir::Ty::Short,
            glsc_hir::DataType::Int => mir::Ty::Int,
            glsc_hir::DataType::Float => mir::Ty::Float,
            glsc_hir::DataType::Double => mir::Ty::Double,
            glsc_hir::DataType::TS18661Float(ts18661_float_type) => todo!(),
            glsc_hir::DataType::Char => mir::Ty::Char,
            glsc_hir::DataType::Long => mir::Ty::Long,
            glsc_hir::DataType::LongLong => mir::Ty::LongLong,
            glsc_hir::DataType::TypedefName(identifier) => {
                let identifier = identifier.into();
                self.get_current_scope()
                    .typedefs
                    .get(&identifier)
                    .expect("Failed to find typedef")
                    .clone()
            }
            glsc_hir::DataType::Pointer(ty) => mir::Ty::Pointer(Box::new(self.lower_ty(ty))),
            glsc_hir::DataType::Function {
                return_type,
                parameters,
            } => todo!(),
            // TODO: add padding to each field
            glsc_hir::DataType::Struct { name, fields } => todo!(),
        }
    }
    pub fn lower_statement(&mut self, statement: &hir::Statement) -> Option<mir::Statement> {
        match statement {
            glsc_hir::Statement::Compound(statements) => {
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
            glsc_hir::Statement::Return(expression) => Some(mir::Statement::Return(
                expression
                    .as_ref()
                    .map(|expression| self.lower_expression(expression)),
            )),
            glsc_hir::Statement::Declaration(declaration) => {
                self.lower_declaration(&declaration);
                None
            }
            glsc_hir::Statement::Expression(None) => None,
            glsc_hir::Statement::Expression(Some(expression)) => Some(mir::Statement::Expression(
                Some(self.lower_expression(expression)),
            )),
            glsc_hir::Statement::If(expression, statement, statement1) => Some(mir::Statement::If(
                self.lower_expression(expression),
                Box::new(self.lower_statement(statement).unwrap()),
                Box::new(
                    statement1
                        .clone()
                        .map(|stmt| self.lower_statement(&stmt).unwrap()),
                ),
            )),
            glsc_hir::Statement::For(for_initializer, condition, update, body) => {
                let mut stmts = vec![];
                let init = match for_initializer {
                    glsc_hir::ForInitializer::Empty => mir::Statement::Empty,
                    glsc_hir::ForInitializer::Declaration(declaration) => {
                        mir::Statement::Declaration(self.lower_declaration2(declaration))
                    }
                    glsc_hir::ForInitializer::Expression(expression) => {
                        mir::Statement::Expression(Some(self.lower_expression(expression)))
                    }
                };

                stmts.push(init);

                let loop_start = self.next_internal_label();
                let loop_end = self.next_internal_label();
                let asdasdasd = mir::Statement::If(
                    self.lower_expression(&condition.as_ref().unwrap()),

                    Box::new(mir::Statement::Compound(vec![
                        self.lower_statement(&body).unwrap(),
                        mir::Statement::Expression(Some(self.lower_expression(&update.as_ref().unwrap()))),
                        mir::Statement::GotoInternal(loop_start)
                    ])),
                    Box::new(Some(mir::Statement::GotoInternal(loop_end))),
                );

                stmts.push(mir::Statement::LabeledStatement(
                    glsc_mir::Label::Internal(self.next_internal_label()),
                    Box::new(asdasdasd),
                ));
                // Empty label technically contains everything after it
                stmts.push(mir::Statement::LabeledStatement(glsc_mir::Label::Internal(loop_end), Box::new(mir::Statement::Empty)));

                Some(mir::Statement::Compound(stmts))
            }
            glsc_hir::Statement::LabeledStatement(label, statement) => todo!(),
            glsc_hir::Statement::Goto(identifier) => {
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
            glsc_hir::Expression::Identifier(identifier) => {
                mir::Expression::Identifier(identifier.into())
            }
            glsc_hir::Expression::BinOp(lhs, binary_operator, rhs) => mir::Expression::BinOp(
                Box::new(self.lower_expression(&lhs)),
                binary_operator.clone(),
                Box::new(self.lower_expression(&rhs)),
            ),
            glsc_hir::Expression::UnaryOp(unary_operator, expression) => mir::Expression::UnaryOp(unary_operator.clone(), Box::new(self.lower_expression(&expression))),
            glsc_hir::Expression::Constant(constant) => mir::Expression::Constant(constant.clone()),
        }
    }
}
