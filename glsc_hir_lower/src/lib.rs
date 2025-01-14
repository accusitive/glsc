use glsc_hir::{self as hir};
use lang_c::{
    ast::{self, SpecifierQualifier, TypeQualifier, TypeSpecifier},
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
        None
    }
    pub fn lower_external_declaration(
        &mut self,
        external_declaration: &ast::ExternalDeclaration,
    ) -> Vec<hir::ExternalDeclaration> {
        match external_declaration {
            ast::ExternalDeclaration::Declaration(node) => {
                self.lower_declaration(&node.node)
            }
            ast::ExternalDeclaration::FunctionDefinition(node) => {
                vec![hir::ExternalDeclaration::FunctionDefinition(
                    self.lower_function_definition(&node.node),
                )]
            }
            _ => unimplemented!(),
        }
    }
    pub fn lower_declaration(&mut self, declaration: &ast::Declaration) -> Vec<hir::ExternalDeclaration> {
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

            external_declarations.push(glsc_hir::ExternalDeclaration::Declaration(declarator_ty, declarator_name));
            // dbg!(&declarator_ty, &declarator_name, init_declarator);
        }
        external_declarations
    }
    pub fn lower_function_definition(
        &mut self,
        function_definition: &ast::FunctionDefinition,
    ) -> hir::FunctionDefinition {
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

        // dbg!(&function_type);
        println!("{}", function_type);
        hir::FunctionDefinition {
            name,
            return_type: function_type.function_return_type(),
            parameters: function_type.function_parameters(),
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

                _ => {}
            };
        }
        ty
    }
}
