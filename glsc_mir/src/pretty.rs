/* Pretty printing code below written by chatgpt :( (im ~~lazy~~ efficient) */
use crate::*;

pub trait VerboseDebugPrinter {
    fn verbose_debug_print(&self, indent: usize);
}

impl VerboseDebugPrinter for Identifier {
    fn verbose_debug_print(&self, indent: usize) {
        println!("{:indent$}Identifier: {}", "", self.0, indent = indent);
    }
}

impl VerboseDebugPrinter for Ty {
    fn verbose_debug_print(&self, indent: usize) {
        match self {
            Ty::None => println!("{:indent$}Type: None", "", indent = indent),
            Ty::Void => println!("{:indent$}Type: Void", "", indent = indent),
            Ty::Short => println!("{:indent$}Type: Short", "", indent = indent),
            Ty::Int => println!("{:indent$}Type: Int", "", indent = indent),
            Ty::Float => println!("{:indent$}Type: Float", "", indent = indent),
            Ty::Double => println!("{:indent$}Type: Double", "", indent = indent),
            Ty::TS18661Float(t) => println!("{:indent$}Type: TS18661Float {:?}", "", t, indent = indent),
            Ty::Char => println!("{:indent$}Type: Char", "", indent = indent),
            Ty::Long => println!("{:indent$}Type: Long", "", indent = indent),
            Ty::LongLong => println!("{:indent$}Type: LongLong", "", indent = indent),
            Ty::TypedefName(id) => {
                println!("{:indent$}Type: TypedefName", "", indent = indent);
                id.verbose_debug_print(indent + 2);
            }
            Ty::Pointer(inner) => {
                println!("{:indent$}Type: Pointer", "", indent = indent);
                inner.verbose_debug_print(indent + 2);
            }
            Ty::Function { return_type, parameters } => {
                println!("{:indent$}Type: Function", "", indent = indent);
                println!("{:indent$}Return Type:", "", indent = indent + 2);
                return_type.verbose_debug_print(indent + 4);
                println!("{:indent$}Parameters:", "", indent = indent + 2);
                for param in parameters {
                    param.verbose_debug_print(indent + 4);
                }
            }
            Ty::Struct { name, fields } => {
                // println!("struct {} {{", name.as_ref().map(|n| n.0.clone()).unwrap_or("".to_string()));
                // for field in fields {
                    // print!("")
                // }
                println!("INCOMPLETE")
            },
            
        }
    }
}

impl VerboseDebugPrinter for FunctionParameter {
    fn verbose_debug_print(&self, indent: usize) {
        println!("{:indent$}Parameter:", "", indent = indent);
        if let Some(name) = &self.name {
            name.verbose_debug_print(indent + 2);
        }
        self.ty.verbose_debug_print(indent + 2);
    }
}

impl VerboseDebugPrinter for FunctionDefinition {
    fn verbose_debug_print(&self, indent: usize) {
        println!("{:indent$}Function:", "", indent = indent);
        self.name.verbose_debug_print(indent + 2);
        println!("{:indent$}Return Type:", "", indent = indent + 2);
        self.return_type.verbose_debug_print(indent + 4);
        println!("{:indent$}Parameters:", "", indent = indent + 2);
        for param in &self.parameters {
            param.verbose_debug_print(indent + 4);
        }
        println!("{:indent$}Body:", "", indent = indent + 2);
        self.body.verbose_debug_print(indent + 4);
    }
}

impl VerboseDebugPrinter for Statement {
    fn verbose_debug_print(&self, indent: usize) {
        match self {
            Statement::Compound(statements) => {
                println!("{:indent$}Compound Statement:", "", indent = indent);
                for stmt in statements {
                    stmt.verbose_debug_print(indent + 2);
                }
            }
            Statement::Return(expr) => {
                println!("{:indent$}Return Statement:", "", indent = indent);
                if let Some(expr) = expr {
                    expr.verbose_debug_print(indent + 2);
                }
            }
            Statement::Declaration(decl) => {
                println!("{:indent$}Declaration Statement:", "", indent = indent);
                decl.verbose_debug_print(indent + 2);
            }
            Statement::Expression(expr) => {
                println!("{:indent$}Expression Statement:", "", indent = indent);
                if let Some(expr) = expr {
                    expr.verbose_debug_print(indent + 2);
                }
            }
            Statement::LabeledStatement(label, stmt) => {
                println!("{:indent$}Labeled Statement:", "", indent = indent);
                label.verbose_debug_print(indent + 2);
                stmt.verbose_debug_print(indent + 2);
            }
            Statement::GotoLabel(label) => {
                println!("{:indent$}Goto Label Statement:", "", indent = indent);
                label.verbose_debug_print(indent + 2);
            }
            Statement::GotoInternal(id) => {
                println!("{:indent$}Goto Internal Statement: {}", "", id, indent = indent);
            }
            Statement::Empty => {
                println!("{:indent$}Empty Statement", "", indent = indent);
            }
            Statement::If(cond, then_stmt, else_stmt) => {
                println!("{:indent$}If Statement:", "", indent = indent);
                println!("{:indent$}Condition:", "", indent = indent + 2);
                cond.verbose_debug_print(indent + 4);
                println!("{:indent$}Then:", "", indent = indent + 2);
                then_stmt.verbose_debug_print(indent + 4);
                if let Some(else_stmt) = else_stmt.as_ref() {
                    println!("{:indent$}Else:", "", indent = indent + 2);
                    else_stmt.verbose_debug_print(indent + 4);
                }
            }
        }
    }
}

impl VerboseDebugPrinter for Declaration {
    fn verbose_debug_print(&self, indent: usize) {
        println!("{:indent$}Declaration:", "", indent = indent);
        self.ty.verbose_debug_print(indent + 2);
        self.name.verbose_debug_print(indent + 2);
        if let Some(init) = &self.init {
            println!("{:indent$}Initializer:", "", indent = indent + 2);
            init.verbose_debug_print(indent + 4);
        }
    }
}

impl VerboseDebugPrinter for Label {
    fn verbose_debug_print(&self, indent: usize) {
        match self {
            Label::Idenitfier(id) => {
                println!("{:indent$}Label: Identifier", "", indent = indent);
                id.verbose_debug_print(indent + 2);
            }
            Label::Case(expr) => {
                println!("{:indent$}Label: Case", "", indent = indent);
                expr.verbose_debug_print(indent + 2);
            }
            Label::Internal(id) => {
                println!("{:indent$}Label: Internal {}", "", id, indent = indent);
            }
            Label::Default => {
                println!("{:indent$}Label: Default", "", indent = indent);
            }
        }
    }
}

impl VerboseDebugPrinter for Expression {
    fn verbose_debug_print(&self, indent: usize) {
        match self {
            Expression::Identifier(id) => {
                println!("{:indent$}Expression: Identifier", "", indent = indent);
                id.verbose_debug_print(indent + 2);
            }
            Expression::BinOp(lhs, op, rhs) => {
                println!("{:indent$}Expression: Binary Operation {:?}", "", op, indent = indent);
                lhs.verbose_debug_print(indent + 2);
                rhs.verbose_debug_print(indent + 2);
            }
            Expression::UnaryOp(op, expr) => {
                println!("{:indent$}Expression: Unary Operation {:?}", "", op, indent = indent);
                expr.verbose_debug_print(indent + 2);
            }
            Expression::Constant(constant) => {
                println!("{:indent$}Expression: Constant {:?}", "", constant, indent = indent);
            }
        }
    }
}


pub trait DebugPrinter {
    fn print(&self, indent: usize);
}

impl DebugPrinter for Identifier {
    fn print(&self, _: usize) {
        print!("{}", self.0);
    }
}

impl DebugPrinter for Ty {
    fn print(&self, _: usize) {
        match self {
            Ty::None => print!("void"),
            Ty::Void => print!("void"),
            Ty::Short => print!("short"),
            Ty::Int => print!("int"),
            Ty::Float => print!("float"),
            Ty::Double => print!("double"),
            Ty::TS18661Float(_) => print!("TS18661Float"), // Simplified
            Ty::Char => print!("char"),
            Ty::Long => print!("long"),
            Ty::LongLong => print!("long long"),
            Ty::TypedefName(name) => name.print(0),
            Ty::Pointer(inner) => {
                inner.print(0);
                print!("*");
            }
            Ty::Function { return_type, parameters } => {
                return_type.print(0);
                print!("(");
                for (i, param) in parameters.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }
                    param.print(0);
                }
                print!(")");
            }
            Ty::Struct { name, fields } => println!("INCOMPLETE"),
            
        }
    }
}

impl DebugPrinter for FunctionParameter {
    fn print(&self, _: usize) {
        self.ty.print(0);
        if let Some(name) = &self.name {
            print!(" ");
            name.print(0);
        }
    }
}

impl DebugPrinter for FunctionDefinition {
    fn print(&self, indent: usize) {
        self.return_type.print(indent);
        print!(" ");
        self.name.print(0);
        print!("(");
        for (i, param) in self.parameters.iter().enumerate() {
            if i > 0 {
                print!(", ");
            }
            param.print(0);
        }
        println!(") {{");
        self.body.print(indent + 4);
        println!("{:indent$}}}", "", indent = indent);
    }
}

impl DebugPrinter for Statement {
    fn print(&self, indent: usize) {
        match self {
            Statement::Compound(statements) => {
                println!("{:indent$}{{", "", indent = indent);
                for stmt in statements {
                    stmt.print(indent + 4);
                }
                println!("{:indent$}}}", "", indent = indent);
            }
            Statement::Return(expr) => {
                print!("{:indent$}return", "", indent = indent);
                if let Some(expr) = expr {
                    print!(" ");
                    expr.print(0);
                }
                println!(";");
            }
            Statement::Declaration(decl) => {
                print!("{:indent$}", "", indent = indent);
                decl.print(0);
                println!(";");
            }
            Statement::Expression(expr) => {
                print!("{:indent$}", "", indent = indent);
                if let Some(expr) = expr {
                    expr.print(0);
                }
                println!(";");
            }
            Statement::LabeledStatement(label, stmt) => {
                label.print(indent);
                println!(":");
                stmt.print(indent + 4);
            }
            Statement::GotoLabel(label) => {
                print!("{:indent$}goto ", "", indent = indent);
                label.print(0);
                println!(";");
            }
            Statement::GotoInternal(id) => {
                println!("{:indent$}goto _internal_{};", "", id, indent = indent);
            }
            Statement::Empty => {
                println!("{:indent$};", "", indent = indent);
            }
            Statement::If(cond, then_stmt, else_stmt) => {
                print!("{:indent$}if (", "", indent = indent);
                cond.print(0);
                println!(") {{");
                then_stmt.print(indent + 4);
                if let Some(else_stmt) = else_stmt.as_ref() {
                    println!("{:indent$}}} else {{", "", indent = indent);
                    else_stmt.print(indent + 4);
                }
                println!("{:indent$}}}", "", indent = indent);
            }
        }
    }
}

impl DebugPrinter for Declaration {
    fn print(&self, _: usize) {
        self.ty.print(0);
        print!(" ");
        self.name.print(0);
        if let Some(init) = &self.init {
            print!(" = ");
            init.print(0);
        }
    }
}

impl DebugPrinter for Label {
    fn print(&self, indent: usize) {
        match self {
            Label::Idenitfier(id) => {
                print!("{:indent$}", "", indent = indent);
                id.print(0);
            }
            Label::Case(expr) => {
                print!("{:indent$}case ", "", indent = indent);
                expr.print(0);
                print!(":");
            }
            Label::Internal(id) => {
                print!("{:indent$}_internal_{}:", "", id, indent = indent);
            }
            Label::Default => {
                print!("{:indent$}default:", "", indent = indent);
            }
        }
    }
}

impl DebugPrinter for Expression {
    fn print(&self, _: usize) {
        match self {
            Expression::Identifier(id) => id.print(0),
            Expression::BinOp(lhs, op, rhs) => {
                lhs.print(0);
                print!(" {:?} ", op);
                rhs.print(0);
            }
            Expression::UnaryOp(op, expr) => {
                print!("{:?}", op);
                expr.print(0);
            }
            Expression::Constant(constant) => {
                match constant {
                    ast::Constant::Integer(integer) => print!("{}", integer.number),
                    ast::Constant::Float(float) => todo!(),
                    ast::Constant::Character(_) => todo!(),
                }
            }
        }
    }
}
