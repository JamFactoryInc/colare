
mod lexer_tests {
    use crate::parser::keyword::Keyword;
    use crate::parser::op::OpType;
    use crate::parser::token::{BracketType, TokenErrorType};
    use crate::tests::Matcher;
    use crate::parser::{self, lexer::{self, Lexer}, token::{LiteralType, Token, TokenType}};


    #[test]
    fn test_number_literal() -> Result<(), ()> {
        Lexer::lex("1")
            .assert_matches(&[
                ("1", TokenType::Literal(LiteralType::Int))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_number_literal_leading_zero() -> Result<(), ()> {
        Lexer::lex("001")
            .assert_matches(&[
                ("1", TokenType::Literal(LiteralType::Int))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_number_literal_long() -> Result<(), ()> {
        Lexer::lex("12345")
            .assert_matches(&[
                ("12345", TokenType::Literal(LiteralType::Int))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_number_literal_zero() -> Result<(), ()> {
        Lexer::lex("0")
            .assert_matches(&[
                ("0", TokenType::Literal(LiteralType::Int))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_number_literal_long_split() -> Result<(), ()> {
        Lexer::lex("1_234_567")
            .assert_matches(&[
                ("1_234_567", TokenType::Literal(LiteralType::Int))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_float_literal() -> Result<(), ()> {
        Lexer::lex("1.")
            .assert_matches(&[
                ("1.", TokenType::Literal(LiteralType::Float))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_float_literal_leading_decimal() -> Result<(), ()> {
        Lexer::lex(".1")
            .assert_matches(&[
                (".1", TokenType::Literal(LiteralType::Float))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_float_literal_leading_zero() -> Result<(), ()> {
        Lexer::lex("0.1")
            .assert_matches(&[
                ("0.1", TokenType::Literal(LiteralType::Float))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_float_literal_leading_zeroes() -> Result<(), ()> {
        Lexer::lex("000.1")
            .assert_matches(&[
                ("0.1", TokenType::Literal(LiteralType::Float))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_float_literal_long() -> Result<(), ()> {
        Lexer::lex("123.123")
            .assert_matches(&[
                ("123.123", TokenType::Literal(LiteralType::Float))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_float_literal_long_split() -> Result<(), ()> {
        Lexer::lex("1_234.123_456")
            .assert_matches(&[
                ("1_234.123_456", TokenType::Literal(LiteralType::Float))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_bin_literal() -> Result<(), ()> {
        Lexer::lex("0b1100")
            .assert_matches(&[
                ("0b1100", TokenType::Literal(LiteralType::Binary))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_bin_literal_upper_b() -> Result<(), ()> {
        Lexer::lex("0B1100")
            .assert_matches(&[
                ("0B1100", TokenType::Literal(LiteralType::Binary))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_bin_literal_split() -> Result<(), ()> {
        Lexer::lex("0b1100_1100")
            .assert_matches(&[
                ("0b1100_1100", TokenType::Literal(LiteralType::Binary))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_hex_literal_upper() -> Result<(), ()> {
        Lexer::lex("0x0123456789ABCDEF")
            .assert_matches(&[
                ("0x0123456789ABCDEF", TokenType::Literal(LiteralType::Hex))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_hex_literal_lower() -> Result<(), ()> {
        Lexer::lex("0x0123456789abcdef")
            .assert_matches(&[
                ("0x0123456789abcdef", TokenType::Literal(LiteralType::Hex))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_hex_literal_split() -> Result<(), ()> {
        Lexer::lex("0x01_23_45")
            .assert_matches(&[
                ("0x01_23_45", TokenType::Literal(LiteralType::Hex))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_hex_literal_upper_x() -> Result<(), ()> {
        Lexer::lex("0X0123456789ABCDEF")
            .assert_matches(&[
                ("0X0123456789ABCDEF", TokenType::Literal(LiteralType::Hex))
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_double_string_literal() -> Result<(), ()> {
        let src = "abc \"123'abc\"";
        Lexer::lex(src)
            .assert_matches(&[
                ("abc", TokenType::Identifier),
                ("\"123'abc\"", TokenType::Literal(LiteralType::DoubleString)),
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_double_string_literal_escaped() -> Result<(), ()> {
        let src = "abc \"12\\\\ \\\"3'abc\"";
        Lexer::lex(src)
            .assert_matches(&[
                ("abc", TokenType::Identifier),
                ("\"12\\\\ \\\"3'abc\"", TokenType::Literal(LiteralType::DoubleString)),
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_single_string_literal() -> Result<(), ()> {
        let src = "abc \'123\"abc\'";
        Lexer::lex(src)
            .assert_matches(&[
                ("abc", TokenType::Identifier),
                ("\'123\"abc\'", TokenType::Literal(LiteralType::SingleString)),
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_single_string_literal_escaped() -> Result<(), ()> {
        let src = "abc \'12\\\\ \\\'3\"abc\'";
        Lexer::lex(src)
            .assert_matches(&[
                ("abc", TokenType::Identifier),
                ("\'12\\\\ \\\'3\"abc\'", TokenType::Literal(LiteralType::SingleString)),
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_keywords() -> Result<(), ()> {
        Lexer::lex("as bool break byte const continue double else float fn for if in int match ref return string struct unt")
            .assert_matches(&[
                ("as", TokenType::Keyword(Keyword::As)),
                ("bool", TokenType::Keyword(Keyword::Bool)),
                ("break", TokenType::Keyword(Keyword::Break)),
                ("byte", TokenType::Keyword(Keyword::Byte)),
                ("const", TokenType::Keyword(Keyword::Const)),
                ("continue", TokenType::Keyword(Keyword::Continue)),
                ("double", TokenType::Keyword(Keyword::Double)),
                ("else", TokenType::Keyword(Keyword::Else)),
                ("float", TokenType::Keyword(Keyword::Float)),
                ("fn", TokenType::Keyword(Keyword::Fn)),
                ("for", TokenType::Keyword(Keyword::For)),
                ("if", TokenType::Keyword(Keyword::If)),
                ("in", TokenType::Keyword(Keyword::In)),
                ("int", TokenType::Keyword(Keyword::Int)),
                ("match", TokenType::Keyword(Keyword::Match)),
                ("ref", TokenType::Keyword(Keyword::Ref)),
                ("return", TokenType::Keyword(Keyword::Return)),
                ("string", TokenType::Keyword(Keyword::String)),
                ("struct", TokenType::Keyword(Keyword::Struct)),
                ("unt", TokenType::Keyword(Keyword::Unt)),
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_idents() -> Result<(), ()> {
        Lexer::lex("abc _abc abc123 abc_abc abc_123 Abc A1 _0 _9")
            .assert_matches(&[
                ("abc", TokenType::Identifier),
                ("_abc", TokenType::Identifier),
                ("abc123", TokenType::Identifier),
                ("abc_abc", TokenType::Identifier),
                ("abc_123", TokenType::Identifier),
                ("Abc", TokenType::Identifier),
                ("A1", TokenType::Identifier),
                ("_0", TokenType::Identifier),
                ("_9", TokenType::Identifier),
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_newline() -> Result<(), ()> {
        let src = "abc\nabc;abc\r";
        Lexer::lex(src)
            .assert_matches(&[
                ("abc", TokenType::Identifier),
                ("\n", TokenType::NewLine),
                ("abc", TokenType::Identifier),
                (";", TokenType::NewLine),
                ("abc", TokenType::Identifier),
                ("\r", TokenType::NewLine),
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_comment() -> Result<(), ()> {
        let src = "abc# abc\ndef";
        Lexer::lex(src)
            .assert_matches(&[
                ("abc", TokenType::Identifier),
                ("\n", TokenType::NewLine),
                ("def", TokenType::Identifier),
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_illegal_escape_double_string() -> Result<(), ()> {
        let src = "\"\\a\"";
        Lexer::lex(src)
            .assert_matches(&[
                ("\\a", TokenType::TokenError(TokenErrorType::UnknownEscapeCharacter)),
                ("\"\\a\"", TokenType::Literal(LiteralType::DoubleString)),
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_illegal_escape_single_string() -> Result<(), ()> {
        let src = "\'\\a\'";
        Lexer::lex(src)
            .assert_matches(&[
                ("\\a", TokenType::TokenError(TokenErrorType::UnknownEscapeCharacter)),
                ("\'\\a\'", TokenType::Literal(LiteralType::SingleString)),
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_range_disambiguation_int_range() -> Result<(), ()> {
        let src = "1.. 1..";
        Lexer::lex(src)
            .assert_matches(&[
                ("1", TokenType::Literal(LiteralType::Int)),
                ("..", TokenType::Operator(OpType::Range)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("..", TokenType::Operator(OpType::Range)),
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_range_disambiguation_int_range_eq() -> Result<(), ()> {
        let src = "1..= 1..=";
        Lexer::lex(src)
            .assert_matches(&[
                ("1", TokenType::Literal(LiteralType::Int)),
                ("..=", TokenType::Operator(OpType::RangeEq)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("..=", TokenType::Operator(OpType::RangeEq)),
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_range_disambiguation_float_range() -> Result<(), ()> {
        let src = "1... 1...";
        Lexer::lex(src)
            .assert_matches(&[
                ("1.", TokenType::Literal(LiteralType::Float)),
                ("..", TokenType::Operator(OpType::Range)),
                ("1.", TokenType::Literal(LiteralType::Float)),
                ("..", TokenType::Operator(OpType::Range)),
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_range_disambiguation_float_range_eq() -> Result<(), ()> {
        let src = "1...= 1...=";
        Lexer::lex(src)
            .assert_matches(&[
                ("1.", TokenType::Literal(LiteralType::Float)),
                ("..=", TokenType::Operator(OpType::RangeEq)),
                ("1.", TokenType::Literal(LiteralType::Float)),
                ("..=", TokenType::Operator(OpType::RangeEq)),
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_range_ident() -> Result<(), ()> {
        let src = "abc .. abc abc..abc abc .. 123 abc..123";
        Lexer::lex(src)
            .assert_matches(&[
                ("abc", TokenType::Identifier),
                ("..", TokenType::Operator(OpType::Range)),
                ("abc", TokenType::Identifier),
                ("abc", TokenType::Identifier),
                ("..", TokenType::Operator(OpType::Range)),
                ("abc", TokenType::Identifier),
                ("abc", TokenType::Identifier),
                ("..", TokenType::Operator(OpType::Range)),
                ("123", TokenType::Literal(LiteralType::Int)),
                ("abc", TokenType::Identifier),
                ("..", TokenType::Operator(OpType::Range)),
                ("123", TokenType::Literal(LiteralType::Int)),
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_end_literal() -> Result<(), ()> {
        let src = "1 1\t1;1\n1(1)1[1]1{1}1,1+1-1*1/1%1|1&1^1!1=1<1>1: ";
        Lexer::lex(src)
            .assert_matches(&[
                ("1", TokenType::Literal(LiteralType::Int)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("1", TokenType::Literal(LiteralType::Int)),
                (";", TokenType::NewLine),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("\n", TokenType::NewLine),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("(", TokenType::Open(BracketType::Paren)),
                ("1", TokenType::Literal(LiteralType::Int)),
                (")", TokenType::Close(BracketType::Paren)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("[", TokenType::Open(BracketType::Square)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("]", TokenType::Close(BracketType::Square)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("{", TokenType::Open(BracketType::Curly)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("}", TokenType::Close(BracketType::Curly)),
                ("1", TokenType::Literal(LiteralType::Int)),
                (",", TokenType::Delimeter),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("+", TokenType::Operator(OpType::Add)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("-", TokenType::Operator(OpType::Sub)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("*", TokenType::Operator(OpType::Mul)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("/", TokenType::Operator(OpType::Div)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("%", TokenType::Operator(OpType::Mod)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("|", TokenType::Operator(OpType::BitOr)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("&", TokenType::Operator(OpType::BitAnd)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("^", TokenType::Operator(OpType::XOr)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("!", TokenType::Operator(OpType::Not)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("=", TokenType::Operator(OpType::Eq)),
                ("1", TokenType::Literal(LiteralType::Int)),
                ("<", TokenType::Operator(OpType::Lt)),
                ("1", TokenType::Literal(LiteralType::Int)),
                (">", TokenType::Operator(OpType::Gt)),
                ("1", TokenType::Literal(LiteralType::Int)),
                (":", TokenType::Operator(OpType::TypeAssign)),
            ]).map_err(|e| panic!("{e}"))
    }

    #[test]
    fn test_end_ident() -> Result<(), ()> {
        let src = "a a\ta;a\na(a)a[a]a{a}a,a+a-a*a/a%a|a&a^a!a=a<a>a:a.a";
        Lexer::lex(src)
            .assert_matches(&[
                ("a", TokenType::Identifier),
                ("a", TokenType::Identifier),
                ("a", TokenType::Identifier),
                (";", TokenType::NewLine),
                ("a", TokenType::Identifier),
                ("\n", TokenType::NewLine),
                ("a", TokenType::Identifier),
                ("(", TokenType::Open(BracketType::Paren)),
                ("a", TokenType::Identifier),
                (")", TokenType::Close(BracketType::Paren)),
                ("a", TokenType::Identifier),
                ("[", TokenType::Open(BracketType::Square)),
                ("a", TokenType::Identifier),
                ("]", TokenType::Close(BracketType::Square)),
                ("a", TokenType::Identifier),
                ("{", TokenType::Open(BracketType::Curly)),
                ("a", TokenType::Identifier),
                ("}", TokenType::Close(BracketType::Curly)),
                ("a", TokenType::Identifier),
                (",", TokenType::Delimeter),
                ("a", TokenType::Identifier),
                ("+", TokenType::Operator(OpType::Add)),
                ("a", TokenType::Identifier),
                ("-", TokenType::Operator(OpType::Sub)),
                ("a", TokenType::Identifier),
                ("*", TokenType::Operator(OpType::Mul)),
                ("a", TokenType::Identifier),
                ("/", TokenType::Operator(OpType::Div)),
                ("a", TokenType::Identifier),
                ("%", TokenType::Operator(OpType::Mod)),
                ("a", TokenType::Identifier),
                ("|", TokenType::Operator(OpType::BitOr)),
                ("a", TokenType::Identifier),
                ("&", TokenType::Operator(OpType::BitAnd)),
                ("a", TokenType::Identifier),
                ("^", TokenType::Operator(OpType::XOr)),
                ("a", TokenType::Identifier),
                ("!", TokenType::Operator(OpType::Not)),
                ("a", TokenType::Identifier),
                ("=", TokenType::Operator(OpType::Eq)),
                ("a", TokenType::Identifier),
                ("<", TokenType::Operator(OpType::Lt)),
                ("a", TokenType::Identifier),
                (">", TokenType::Operator(OpType::Gt)),
                ("a", TokenType::Identifier),
                (":", TokenType::Operator(OpType::TypeAssign)),
                ("a", TokenType::Identifier),
                (".", TokenType::Operator(OpType::Access)),
                ("a", TokenType::Identifier),
            ]).map_err(|e| panic!("{e}"))
    }
}


