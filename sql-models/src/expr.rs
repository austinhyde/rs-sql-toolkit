use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    UnOp(UnaryOperation),
    BinOp(BinaryOperation),
    FieldRef(String),
    StrLit(String),
    NumLit(f32),
    BoolLit(bool),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnOp(op) => write!(f, "{}", op),
            Self::BinOp(op) => write!(f, "{}", op),
            Self::FieldRef(val) => write!(f, "{}", val),
            Self::BoolLit(val) => write!(f, "{}", val),
            Self::NumLit(val) => write!(f, "{}", val),
            Self::StrLit(val) => write!(f, "'{}'", val), // TODO escapes
        }
    }
}

pub trait ExprContext {
    fn get_field_value<T>(&self, name: &str) -> T;
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperation {
    pub op: UnOp,
    pub rhs: Box<Expr>,
}
impl UnaryOperation {
    pub fn new(op: UnOp, rhs: Expr) -> Self {
        Self {
            op,
            rhs: Box::new(rhs),
        }
    }
    pub fn new_tuple((op, rhs): (UnOp, Expr)) -> Self {
        Self {
            op,
            rhs: Box::new(rhs),
        }
    }
}
impl Display for UnaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {})", self.op, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperation {
    pub op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}
impl BinaryOperation {
    pub fn new(lhs: Expr, op: BinOp, rhs: Expr) -> Self {
        Self {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    pub fn new_tuple((lhs, op, rhs): (Expr, BinOp, Expr)) -> Self {
        Self {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}
impl Display for BinaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.lhs, self.op, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Not,
}
impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnOp::Not => "NOT",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    And,
    Or,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
}

impl BinOp {
    /// Higher precedence means it will be "done first"
    pub fn precedence(&self) -> u8 {
        match self {
            Self::And | Self::Or => 1,
            _ => 2,
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::And => "AND",
                Self::Or => "OR",
                Self::Lt => "<",
                Self::Le => "<=",
                Self::Gt => ">",
                Self::Ge => ">=",
                Self::Eq => "=",
                Self::Ne => "!=",
            }
        )
    }
}
