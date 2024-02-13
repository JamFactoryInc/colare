
mod lexer_tests {
    use crate::parser::keyword::Keyword;
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
        Lexer::lex("as bool break byte const continue double else float fn for if in int ref return string struct unt")
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
}


