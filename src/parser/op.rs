
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OpType {
    Add,
    AddEq,
    Sub,
    SubEq,
    Mul,
    MulEq,
    Div,
    DivEq,
    Mod,
    ModEq,
    And,
    BitAnd,
    BitAndEq,
    Or,
    BitOr,
    BitOrEq,
    XOr,
    XOrEq,
    Not,
    NotEq,
    Eq,
    DoubleEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Shr,
    Shl,
    Range,
    RangeEq,
    TypeAssign,
    Access,
    Err
}

impl OpType {
    #[inline(always)]
    pub const fn from_single(c: char) -> OpType {
        match c {
            '+' => OpType::Add,
            '-' => OpType::Sub,
            '*' => OpType::Mul,
            '/' => OpType::Div,
            '%' => OpType::Mod,
            '&' => OpType::BitAnd,
            '|' => OpType::BitOr,
            '^' => OpType::XOr,
            '=' => OpType::Eq,
            '<' => OpType::Gt,
            '>' => OpType::Lt,
            ':' => OpType::TypeAssign,
            '.' => OpType::Access,
            _ => OpType::Err
        }
    }

    #[inline(always)]
    pub const fn before_eq(op_type: OpType) -> OpType {
        match op_type {
            OpType::Add => OpType::AddEq,
            OpType::Sub => OpType::SubEq,
            OpType::Mul => OpType::MulEq,
            OpType::Div => OpType::DivEq,
            OpType::Mod => OpType::ModEq,
            OpType::BitAnd => OpType::BitAndEq,
            OpType::Or => OpType::BitOrEq,
            OpType::XOr => OpType::XOrEq,
            OpType::Not => OpType::NotEq,
            OpType::Eq => OpType::DoubleEq,
            _ => OpType::Err,
        }
    }
}