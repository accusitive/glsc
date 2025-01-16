use std::fmt::{Display, Pointer};

use lang_c::ast::{self, Identifier, StorageClassSpecifier, TS18661FloatType};

#[derive(Debug)]
pub struct TranslationUnit {
    pub declarations: Vec<ExternalDeclaration>,
}
#[derive(Debug)]
pub enum ExternalDeclaration {
    FunctionDefinition(FunctionDefinition),
    Declaration(Declaration)
}
#[derive(Debug, Clone)]
pub struct Declaration {
    pub ty: Ty,
    pub name: Identifier,
    pub init: Option<Expression>
}
#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: ast::Identifier,
    pub return_type: Ty,
    pub parameters: Vec<Parameter>,
    pub body: Statement
}

#[derive(Debug, Clone)]
pub enum DataType {
    None,
    Void,
    Short,
    Int,
    Float,
    Double,
    TS18661Float(TS18661FloatType),
    Char,
    Long,
    LongLong,
    TypedefName(Identifier),
    Pointer(Box<Ty>),
    Function {
        return_type: Box<Ty>,
        parameters: Vec<Parameter>,
    },
    Struct {
        name: Option<ast::Identifier>,
        fields: Vec<StructField>
    }
}
#[derive(Debug, Clone)]
pub struct StructField {
    pub name: ast::Identifier,
    pub ty: Ty
}
#[derive(Debug, Clone)]
pub struct Ty {
    pub constness: Option<bool>,
    pub signedness: Option<bool>,
    pub storage_class: Option<StorageClassSpecifier>,
    pub data_type: DataType,
}
#[derive(Debug, Clone)]
pub struct Parameter {
    pub ty: Ty,
    pub name: Option<ast::Identifier>,
}
#[derive(Debug, Clone)]
pub enum ForInitializer {
    Empty, 
    Declaration(Declaration),
    Expression(Expression),
}
#[derive(Debug, Clone)]
pub enum Statement {
    Compound(Vec<Self>),
    Return(Option<Expression>),
    Declaration(Declaration),
    Expression(Option<Expression>),
    If(Expression, Box<Statement>, Box<Option<Statement>>),
    For(ForInitializer, Option<Expression>, Option<Expression>, Box<Statement>),
    LabeledStatement(Label, Box<Self>),
    Goto(ast::Identifier)
}
#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    BinOp(Box<Self>, ast::BinaryOperator, Box<Self>),
    UnaryOp(ast::UnaryOperator, Box<Self>),
    Constant(ast::Constant)
}
#[derive(Debug, Clone)]
pub enum Label {
    Idenitfier(Identifier),
    Case(Expression),
    Default
}
impl Ty {
    pub fn new(data_type: DataType) -> Self {
        Self {
            constness: None,
            signedness: None,
            storage_class: None,
            data_type,
        }
    }
    pub fn signed(&mut self) -> Option<()> {
        if self.signedness.is_some() {
            return None;
        }
        self.signedness = Some(true);
        Some(())
    }
    pub fn unsigned(&mut self) -> Option<()> {
        if self.signedness.is_some() {
            return None;
        }
        self.signedness = Some(false);
        Some(())
    }
    pub fn const_(&mut self) -> Option<()> {
        if self.constness.is_some() {
            return None;
        }
        self.constness = Some(true);
        Some(())
    }
    pub fn short(&mut self) -> Option<()> {
        self.data_type = DataType::Short;
        Some(())
    }
    pub fn int(&mut self) -> Option<()> {
        self.data_type = DataType::Int;
        Some(())
    }
    pub fn float(&mut self) -> Option<()> {
        self.data_type = DataType::Float;
        Some(())
    }
    pub fn double(&mut self) -> Option<()> {
        self.data_type = DataType::Double;
        Some(())
    }
    pub fn ts18661float(&mut self, float_type: TS18661FloatType) -> Option<()> {
        self.data_type = DataType::TS18661Float(float_type);
        Some(())
    }
    pub fn char(&mut self) -> Option<()> {
        self.data_type = DataType::Char;
        Some(())
    }
    pub fn ptr(&mut self) -> Option<()> {
        self.data_type = DataType::Pointer(Box::new(self.clone()));
        Some(())
    }
    pub fn function(&mut self, params: Vec<Parameter>) -> Option<()> {
        dbg!(&self, "setting self to function");
        self.data_type = DataType::Function {
            return_type: Box::new(self.clone()),
            parameters: params,
        };
        Some(())
    }
    pub fn struct_(&mut self, struct_name: Option<ast::Identifier>, struct_fields: Vec<StructField>) -> Option<()> {
        self.data_type = DataType::Struct { name: struct_name, fields: struct_fields };
        Some(())
    }
    pub fn void(&mut self) -> Option<()> {
        self.data_type = DataType::Void;
        Some(())
    }
    pub fn long(&mut self) -> Option<()> {
        if self.is_long() {
            self.data_type = DataType::LongLong;
        } else {
            self.data_type = DataType::Long;
        }

        Some(())
    }
    pub fn typedef(&mut self) -> Option<()> {
        if self.storage_class.is_some() {
            None
        } else {
            self.storage_class = Some(StorageClassSpecifier::Typedef);
            Some(())
        }
    }
    pub fn extern_(&mut self) -> Option<()> {
        if self.storage_class.is_some() {
            None
        } else {
            self.storage_class = Some(StorageClassSpecifier::Extern);
            Some(())
        }
    }
    pub fn typedef_name(&mut self, typedef_name: Identifier) -> Option<()> {
        self.data_type = DataType::TypedefName(typedef_name);
        Some(())
    }
    pub fn is_long(&self) -> bool {
        match self.data_type {
            DataType::Long => true,
            _ => false,
        }
    }
    pub fn is_function(&self) -> bool {
        match self.data_type {
            DataType::Function { ..} => true,
            _ => false,
        }
    }
    pub fn is_typedef(&self) -> bool {
        match self.storage_class {
            Some(StorageClassSpecifier::Typedef) => true,
            _ => false
        }
    }
    pub fn function_return_type(&self) -> Ty {
        match &self.data_type {
            DataType::Function {
                return_type: ret, ..
            } => *ret.clone(),
            _ => panic!("function_return_type called on non function type"),
        }
    }
    pub fn function_parameters(&self) -> Vec<Parameter> {
        match &self.data_type {
            DataType::Function {
                return_type: _ret,
                parameters,
            } => parameters.clone(),
            _ => panic!("function_return_type called on non function type"),
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.constness.is_some() && self.constness.unwrap() == true {
            write!(f, "const ")?;
        }
        if self.signedness.is_some() && self.signedness.unwrap() == true {
            write!(f, "signed ")?;
        }
        write!(f, "{}", self.data_type)
    }
}
impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::None => write!(f, "NONE"),
            DataType::Void => write!(f, "void"),
            DataType::Short => write!(f, "short"),
            DataType::Int => write!(f, "int"),
            DataType::Float => write!(f, "float"),
            DataType::Double => write!(f, "double"),

            DataType::TS18661Float(float_type) => write!(f, "TS18661Float<{:?}>", float_type),

            DataType::Char => write!(f, "char"),
            DataType::Long => write!(f, "long"),
            DataType::LongLong => write!(f, "long long"),
            DataType::TypedefName(typedef_name) => write!(f, "{}", typedef_name.name),
            DataType::Pointer(ty) => write!(f, "{}*", ty),
            DataType::Struct { name, fields } => {
                let name = name.as_ref().unwrap_or(&Identifier{name: String::new()}).name.clone();
                write!(f, "struct {}{{", name)?;

                for field in fields {
                    write!(f, "{} {};", field.ty, field.name.name)?;
                }
                write!(f, "}}")

            },
            DataType::Function {
                return_type,
                parameters,
            } => {
                write!(f, "fn(")?;
                for param in parameters {
                    write!(f, "{} {}, ", param.ty, param.name.as_ref().unwrap_or(&ast::Identifier{name: String::new()}).name)?;
                }

                write!(f, ") -> {}", return_type)
            }
        }
    }
}
