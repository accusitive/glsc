use glsc_hir as hir;
use lang_c::ast as ast;

pub mod pretty;
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Identifier(pub String);

impl Into<crate::Identifier> for ast::Identifier {
    fn into(self) -> crate::Identifier {
        crate::Identifier(self.name)
    }
}
impl Into<crate::Identifier> for &ast::Identifier {
    fn into(self) -> crate::Identifier {
        crate::Identifier(self.name.clone())
    }
}
#[derive(Debug, Clone)]
pub enum Ty {
    None,
    Void,
    Short,
    Int,
    Float,
    Double,
    TS18661Float(lang_c::ast::TS18661FloatType),
    Char,
    Long,
    LongLong,
    TypedefName(Identifier),
    Pointer(Box<Ty>),
    Function {
        return_type: Box<Self>,
        parameters: Vec<FunctionParameter>,
    },
    // Struct {
    //     name: Option<Identifier>,
    //     fields: Vec<StructField>
    // }
}
#[derive(Debug, Clone)]
pub struct FunctionParameter {
    pub name: Option<Identifier>,
    pub ty: Ty,
}
#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Ty,
    pub body: Statement
}
#[derive(Debug)]
pub enum ExternalDeclaration {
    FunctionDefinition(FunctionDefinition),
    Declaration(Declaration)
}
#[derive(Debug)]
pub struct Declaration {
    pub ty: Ty,
    pub name: Identifier,
    pub init: Option<Expression>
}

#[derive(Debug)]
pub enum Statement {
    Compound(Vec<Self>),
    Return(Option<Expression>),
    Declaration(Declaration),
    Expression(Option<Expression>),
    LabeledStatement(Label, Box<Self>),
    GotoLabel(Identifier),
    GotoInternal(u64),
    Empty,
    If(Expression, Box<Statement>, Box<Option<Statement>>),
}
#[derive(Debug)]
pub enum Label {
    Idenitfier(Identifier),
    Case(Expression),
    Internal(u64),
    Default
}
#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    BinOp(Box<Self>, ast::BinaryOperator, Box<Self>),
    UnaryOp(ast::UnaryOperator, Box<Self>),
    Constant(ast::Constant)
}

