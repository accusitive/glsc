use glsc_hir::{self as hir, ExternalDeclaration};
use lang_c::{
    ast::{self, Initializer, SpecifierQualifier, TypeQualifier, TypeSpecifier},
    span::Node,
};

pub struct HirLower {}
impl HirLower {
    pub fn lower_translation_unit(
        &mut self,
        tu: &ast::TranslationUnit,
    ) -> Option<hir::TranslationUnit> {
        let mut external_declarations = vec![];

        for external_declaration in &tu.0 {
            external_declarations.push(self.lower_external_declaration(&external_declaration.node))
        }

        dbg!(&external_declarations);
        Some(hir::TranslationUnit{
            declarations: external_declarations.into_iter().flatten().collect::<Vec<_>>()
        })
    }
    pub fn lower_external_declaration(
        &mut self,
        external_declaration: &ast::ExternalDeclaration,
    ) -> Vec<hir::ExternalDeclaration> {
        match external_declaration {
            ast::ExternalDeclaration::Declaration(node) => self.lower_declaration(&node.node),
            ast::ExternalDeclaration::FunctionDefinition(node) => {
                vec![hir::ExternalDeclaration::FunctionDefinition(
                    self.lower_function_definition(&node.node),
                )]
            }
            _ => unimplemented!(),
        }
    }
    pub fn lower_declaration(
        &mut self,
        declaration: &ast::Declaration,
    ) -> Vec<hir::ExternalDeclaration> {
        let mut external_declarations = vec![];

        let declaration_type = self
            .parse_declaration_specifiers(&declaration.specifiers)
            .unwrap();
        for init_declarator in &declaration.declarators {
            let declarator_name = self
                .get_declarator_identifier(&init_declarator.node.declarator.node)
                .unwrap();
            let declarator_ty = self.parse_derived_declarators(
                &declaration_type,
                &init_declarator.node.declarator.node.derived,
            );

            let mut init = None;
            match &init_declarator.node.initializer {
                Some(Node { node: Initializer::Expression(e), span: _ }) => {
                    init = Some(self.lower_expression(&e.node));
                },
                _ => {}
            }

            external_declarations.push(glsc_hir::ExternalDeclaration::Declaration(hir::Declaration{
                ty: declarator_ty,
                name: declarator_name,
                init,
        }));
            // dbg!(&declarator_ty, &declarator_name, init_declarator);
        }
        external_declarations
    }
    pub fn lower_function_definition(
        &mut self,
        function_definition: &ast::FunctionDefinition,
    ) -> hir::FunctionDefinition {
        assert!(function_definition.declarations.len() == 0, "no support for k&r style functions");
        let name = self
            .get_declarator_identifier(&function_definition.declarator.node)
            .expect("Function definition name is not an identifier");

        let mut declaration_specifiers = self
            .parse_declaration_specifiers(&function_definition.specifiers)
            .expect("Failed to parse function type specifiers");

        // Not sure if this is correct. If the function declarator isnt an identifier, I parse the derived declarators onto the current type.
        if let ast::DeclaratorKind::Declarator(node) =
            &function_definition.declarator.node.kind.node
        {
            declaration_specifiers =
                self.parse_derived_declarators(&declaration_specifiers, &node.node.derived);
            if let ast::DeclaratorKind::Declarator(node) = &node.node.kind.node {
                panic!("no support for function pointer to function pointer")
            }
        }
        let function_type = self.parse_derived_declarators(
            &declaration_specifiers,
            &function_definition.declarator.node.derived,
        );

        let body = self.lower_statement(&function_definition.statement.node);
        // dbg!(&function_type);
        println!("{}", function_type);
        hir::FunctionDefinition {
            name,
            return_type: function_type.function_return_type(),
            parameters: function_type.function_parameters(),
            body
        }
    }
    pub fn get_declarator_identifier(
        &mut self,
        declarator: &ast::Declarator,
    ) -> Option<ast::Identifier> {
        match &declarator.kind.node {
            ast::DeclaratorKind::Identifier(node) => Some(node.node.clone()),
            ast::DeclaratorKind::Declarator(node) => self.get_declarator_identifier(&node.node),
            ast::DeclaratorKind::Abstract => None,
        }
    }
    pub fn parse_declaration_specifiers(
        &mut self,
        declaration_specifiers: &[Node<ast::DeclarationSpecifier>],
    ) -> Option<hir::Ty> {
        let mut current = hir::Ty::new(hir::DataType::None);

        for declaration_specifier in declaration_specifiers {
            match &declaration_specifier.node {
                ast::DeclarationSpecifier::StorageClass(node) => {
                    match node.node {
                        ast::StorageClassSpecifier::Typedef => {
                            current.typedef()?;
                        }
                        ast::StorageClassSpecifier::Extern => {
                            current.extern_()?;
                        }
                        // ast::StorageClassSpecifier::Static => todo!(),
                        _ => todo!(),
                    }
                }
                ast::DeclarationSpecifier::TypeSpecifier(type_specifier) => {
                    self.apply_type_specifier(&type_specifier.node, &mut current);
                }
                ast::DeclarationSpecifier::TypeQualifier(type_qualifier) => {
                    self.apply_type_qualifier(&type_qualifier.node, &mut current);
                }
                ast::DeclarationSpecifier::Function(node) => todo!(),
                ast::DeclarationSpecifier::Alignment(node) => todo!(),
                ast::DeclarationSpecifier::Extension(vec) => todo!(),
            }
        }
        Some(current)
    }
    pub fn apply_type_specifier(
        &mut self,
        type_specifier: &TypeSpecifier,
        ty: &mut hir::Ty,
    ) -> Option<()> {
        match &type_specifier {
            ast::TypeSpecifier::Void => ty.void()?,
            ast::TypeSpecifier::Short => ty.int()?,

            ast::TypeSpecifier::Int => ty.int()?,
            ast::TypeSpecifier::Long => ty.long()?,
            ast::TypeSpecifier::Signed => ty.signed()?,
            ast::TypeSpecifier::Unsigned => ty.unsigned()?,
            ast::TypeSpecifier::Char => ty.char()?,
            ast::TypeSpecifier::Float => ty.float()?,
            ast::TypeSpecifier::Double => ty.double()?,

            ast::TypeSpecifier::TS18661Float(float_type) => ty.ts18661float(float_type.clone())?,

            ast::TypeSpecifier::Struct(struct_type) => {
                let struct_identifier =
                    struct_type.node.identifier.as_ref().map(|i| i.node.clone());

                let mut struct_fields = vec![];

                if let Some(declarations) = &struct_type.node.declarations {
                    for declaration in declarations {
                        // dbg!(&declaration);

                        match &declaration.node {
                            ast::StructDeclaration::Field(struct_field) => {
                                // Not the full field type, since multiple variable can be declared at once, but they arent necessarily all the same Ty. (some can be pointers,arrays,etc)
                                let declaration_type =
                                    self.parse_specifier_qualifiers(&struct_field.node.specifiers);
                                for struct_declarator in &struct_field.node.declarators {
                                    // not handled yet
                                    assert!(struct_declarator.node.bit_width.is_none());

                                    if let Some(struct_declarator) =
                                        &struct_declarator.node.declarator
                                    {
                                        let field_type = self.parse_derived_declarators(
                                            &declaration_type,
                                            &struct_declarator.node.derived,
                                        );
                                        let struct_name = self
                                            .get_declarator_identifier(&struct_declarator.node)
                                            .unwrap();
                                        struct_fields.push(hir::StructField {
                                            ty: field_type,
                                            name: struct_name,
                                        });
                                    }
                                }
                                // dbg!(&declaration_type);
                            }
                            ast::StructDeclaration::StaticAssert(node) => unimplemented!(),
                        }
                    }
                }
                ty.struct_(struct_identifier, struct_fields)?;
            }
            ast::TypeSpecifier::TypedefName(name) => {
                ty.typedef_name(name.node.clone())?;
            }
            _ => unimplemented!("{:#?} ", type_specifier),
        }
        Some(())
    }
    pub fn apply_type_qualifier(
        &mut self,
        type_qualifier: &TypeQualifier,
        ty: &mut hir::Ty,
    ) -> Option<()> {
        match type_qualifier {
            ast::TypeQualifier::Const => ty.const_()?,
            _ => unimplemented!(),
        }
        Some(())
    }
    pub fn parse_specifier_qualifiers(
        &mut self,
        specifier_qualifiers: &[Node<SpecifierQualifier>],
    ) -> hir::Ty {
        let mut field_ty = hir::Ty::new(hir::DataType::None);

        for specifier_qualifier in specifier_qualifiers {
            match &specifier_qualifier.node {
                SpecifierQualifier::TypeSpecifier(node) => {
                    self.apply_type_specifier(&node.node, &mut field_ty);
                }
                SpecifierQualifier::TypeQualifier(node) => todo!(),
                SpecifierQualifier::Extension(vec) => todo!(),
            }
        }
        field_ty
    }
    pub fn parse_derived_declarators(
        &mut self,
        ty: &hir::Ty,
        derived_declarators: &[Node<ast::DerivedDeclarator>],
    ) -> hir::Ty {
        let mut ty = ty.clone();
        for derived_declarator in derived_declarators {
            match &derived_declarator.node {
                ast::DerivedDeclarator::Pointer(_pointer_qualifiers) => {
                    ty.ptr();
                }
                ast::DerivedDeclarator::Function(function_declarator) => {
                    let mut parameters = vec![];

                    for parameter_declaration in &function_declarator.node.parameters {
                        let parameter_type_specifiers = self
                            .parse_declaration_specifiers(&parameter_declaration.node.specifiers)
                            .expect("Failed to parse declaration specifiers for parameter");

                        if let Some(declarator) = &parameter_declaration.node.declarator {
                            let parameter_type = self.parse_derived_declarators(
                                &parameter_type_specifiers,
                                &declarator.node.derived,
                            );
                            let parameter_name = self.get_declarator_identifier(&declarator.node);

                            parameters.push(hir::Parameter {
                                ty: parameter_type,
                                name: parameter_name,
                            })
                        } else {
                            parameters.push(hir::Parameter {
                                ty: parameter_type_specifiers,
                                name: None,
                            })
                        }
                    }

                    ty.function(parameters);
                }
                ast::DerivedDeclarator::KRFunction(vec) => panic!("K&R functions not supported"),
                _ => unimplemented!()
                
            };
        }
        ty
    }

    pub fn lower_statement(&mut self, statement: &ast::Statement) -> hir::Statement{
        match statement {
            ast::Statement::Labeled(labeled_statement) => {
                let label = match &labeled_statement.node.label.node {
                    ast::Label::Identifier(node) => hir::Label::Idenitfier(node.node.clone()),
                    ast::Label::Case(node) => hir::Label::Case(self.lower_expression(&node.node)),
                    ast::Label::Default => hir::Label::Default,

                    ast::Label::CaseRange(_) => unimplemented!("Unsupported extension"),
                };

                hir::Statement::LabeledStatement(label, Box::new(self.lower_statement(&labeled_statement.node.statement.node)))
            },
            ast::Statement::Compound(block_items) => {
                let mut statements = vec![];
                for block_item in block_items {
                    match &block_item.node {
                        ast::BlockItem::Declaration(declaration) => {
                            for external_declaration in self.lower_declaration(&declaration.node) {
                                if let ExternalDeclaration::Declaration(declaration) = external_declaration {
                                    statements.push(glsc_hir::Statement::Declaration(declaration));
                                }
                            }
                        },
                        ast::BlockItem::StaticAssert(node) => todo!(),
                        ast::BlockItem::Statement(statement) => {
                            statements.push(self.lower_statement(&statement.node))
                        }
                    }
                }
                hir::Statement::Compound(statements)
            }
            ast::Statement::Expression(node) => {
                if let Some(expression) = node {
                    hir::Statement::Expression(Some(self.lower_expression(&expression.node)))
                } else {
                    hir::Statement::Expression(None)
                }
            },
            ast::Statement::If(if_statement) => {
                let condition = self.lower_expression(&if_statement.node.condition.node);
                let then = self.lower_statement(&if_statement.node.then_statement.node);
                let elze = if_statement.node.else_statement.as_ref().map(|stmt| self.lower_statement(&stmt.node));
                hir::Statement::If(condition, Box::new(then), Box::new(elze))
            },
            ast::Statement::Switch(node) => todo!(),
            ast::Statement::While(node) => todo!(),
            ast::Statement::DoWhile(node) => todo!(),
            ast::Statement::For(for_statement) => {
                let for_initializer = match &for_statement.node.initializer.node {
                    ast::ForInitializer::Empty => hir::ForInitializer::Empty,
                    ast::ForInitializer::Expression(node) => hir::ForInitializer::Expression(self.lower_expression(&node.node)),
                    ast::ForInitializer::Declaration(node) => {
                         let d = self.lower_declaration(&node.node); 
                        assert!(d.len() == 1); 
                        if let ExternalDeclaration::Declaration(d) = &d[0] {
                            hir::ForInitializer::Declaration(d.clone())
                        } else {
                            panic!("i dont even know.")
                        }
                    },
                    ast::ForInitializer::StaticAssert(node) => unimplemented!(),
                };
                let condition = for_statement.node.condition.as_ref().map(|expr| self.lower_expression(&expr.node));
                let step = for_statement.node.step.as_ref().map(|expr| self.lower_expression(&expr.node));

                let stmt = self.lower_statement(&for_statement.node.statement.node);
                hir::Statement::For(for_initializer, condition, step, Box::new(stmt))
            },
            ast::Statement::Goto(node) => hir::Statement::Goto(node.node.clone()),
            ast::Statement::Continue => todo!(),
            ast::Statement::Break => todo!(),
            ast::Statement::Return(expression) => {
                if let Some(expression) = expression {
                    hir::Statement::Return(Some(self.lower_expression(&expression.node)))
                } else {
                    hir::Statement::Return(None)
                }
            },
            ast::Statement::Asm(node) => todo!(),
        }
    }
    pub fn lower_expression(&mut self, expression: &ast::Expression) -> hir::Expression {
        match expression {
            ast::Expression::Identifier(identifier) => hir::Expression::Identifier(identifier.node.clone()),
            ast::Expression::Constant(constant) => hir::Expression::Constant(constant.node.clone()),
            ast::Expression::StringLiteral(node) => todo!(),
            ast::Expression::GenericSelection(node) => todo!(),
            ast::Expression::Member(node) => todo!(),
            ast::Expression::Call(node) => todo!(),
            ast::Expression::CompoundLiteral(node) => todo!(),
            ast::Expression::SizeOfTy(node) => todo!(),
            ast::Expression::SizeOfVal(node) => todo!(),
            ast::Expression::AlignOf(node) => todo!(),
            ast::Expression::UnaryOperator(node) => {
                let operand = self.lower_expression(&node.node.operand.node);
                let operator =  node.node.operator.node.clone();

                hir::Expression::UnaryOp(operator, Box::new(operand))

            },
            ast::Expression::Cast(node) => todo!(),
            ast::Expression::BinaryOperator(expression) => {
                let lhs = self.lower_expression(&expression.node.lhs.node);
                let rhs = self.lower_expression(&expression.node.rhs.node);

                hir::Expression::BinOp(Box::new(lhs), expression.node.operator.node.clone(), Box::new(rhs))
            },
            ast::Expression::Conditional(node) => todo!(),
            ast::Expression::Comma(vec) => todo!(),
            ast::Expression::OffsetOf(node) => todo!(),
            ast::Expression::VaArg(node) => todo!(),
            ast::Expression::Statement(node) => todo!(),
        }
    }
 }
