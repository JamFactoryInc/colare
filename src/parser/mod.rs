use self::{identifiers::IdentifierManager, scope::ScopeManager};


pub mod identifiers;
pub mod scope;
pub mod lexer;

pub struct Parser<'src> {
    source: &'src str,
    ident_manager: IdentifierManager<'src>,
    scope_manaeger: ScopeManager,
}

#[derive(Clone, Copy)]
pub enum BracketType {
    Paren,
    Curly,
    Square
}

#[derive(Clone, Copy)]
pub enum LiteralType {
    Char,
    String,
    Int,
    Float,
    Hex,
    Binary,
    Bool,
}

#[derive(Clone, Copy)]
pub enum TokenType {
    Keyword,
    Literal(LiteralType),
    Operator(OpType),
    Identifier,
    Delimeter,
    DoubleString,
    SingleString,
    Open(BracketType),
    Close(BracketType),
    NewLine,
    TokenError,
}

impl TokenType {
    #[inline(always)]
    pub const fn from_single(c: char) -> TokenType {
        match c {
            '(' => TokenType::Open(BracketType::Paren),
            ')' => TokenType::Close(BracketType::Paren),
            '[' => TokenType::Open(BracketType::Square),
            ']' => TokenType::Close(BracketType::Square),
            '{' => TokenType::Open(BracketType::Curly),
            '}' => TokenType::Close(BracketType::Curly),
            '\n' => TokenType::NewLine,
            ',' => TokenType::Delimeter,
            _ => TokenType::TokenError
        }
    }
}

#[derive(Clone, Copy)]
pub enum OpType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Not,
    Eq,
    Lt,
    Gt,
    Range,
    RangeInclusive,
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
            '&' => OpType::And,
            '|' => OpType::Or,
            '^' => OpType::Xor,
            '=' => OpType::Eq,
            '<' => OpType::Gt,
            '>' => OpType::Lt,
            _ => OpType::Err
        }
    }
}

#[derive(Clone, Copy)]
pub struct Span<'src> {
    contents: &'src str,
    line_number: u32,
    column_number: u32,
}

#[derive(Clone, Copy)]
pub struct Token<'src> {
    span: Span<'src>,
    token_type: TokenType
}

#[derive(Default, Clone)]
pub struct TokenStream<'src> {
    stream: Vec<Token<'src>>
}

#[derive(Clone, Copy)]
pub enum BlockType {
    /// Surrounded by {}
    Block,
    /// Surrounded by () or a complete expression
    Expression,
    /// Surrounded by []
    Slice,
    /// Surrounded by <>
    Type,
}

#[derive(Clone, Copy)]
pub enum ParseTokenVariant<'src> {
    Directive(ParseDirectiveType),
    EndDirective(ParseDirectiveType),
    Leaf { leaf_type: ParseLeafType, token: Token<'src> },
    ParseError(ParseErrorVariant<'src>)
}

#[derive(Clone, Copy)]
pub enum ParseLeafType {
    IdentDeclaration,
    Ident,
    Type,
}

#[derive(Clone, Copy)]
pub enum ParseDirectiveType {
    /// <ident_decl> : <type?> = <expression>
    VariableDeclaration,
    /// <ident> = <expression>
    VariableAssignment,
    /// <vis?> fn <ident_decl>(<function_params?>) : <type>
    FunctionDeclaration,
    /// <<<ident> : <type>>*>
    FunctionParams,
    /// 
    TypeDeclaration,
    StartBlock(BlockType),
    EndBlock(BlockType),
}

#[derive(Clone, Copy)]
pub enum ParseErrorVariant<'src> {
    TokenError(Span<'src>),
    MessageProvider(fn(Span<'src>) -> String)
}