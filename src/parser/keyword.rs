
pub enum KeywordMatchType {
    None,
    Complete,
    Partial,
}

macro_rules! keywords {
    ($($kw:ident,)+) => {
        use paste::paste;

        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum Keyword {
            $(
                $kw
            ),+
        }

        impl Keyword {
            pub fn matches(self, index: usize, c: char) -> KeywordMatchType {
                let keyword = match self {
                    $(
                        Keyword:: $kw => paste! { stringify!( [<$kw:lower>] ) }
                    ),+
                };
                
                let matches = keyword.chars()
                    .nth(index as usize)
                    .map_or(false, |keyword_char| keyword_char == c);
                
                match matches {
                    true if keyword.len() - 1 == index => KeywordMatchType::Complete,
                    true => KeywordMatchType::Partial,
                    _ => KeywordMatchType::None,
                }
            }

            pub fn is_keyword(kw: &str) -> Option<Keyword> {
                match kw {
                    $(
                        paste! { stringify!( [<$kw:lower>] ) } => Some(Keyword:: $kw),
                    )+
                    _ => None,
                }
            }
        }

        pub const KEYWORDS: &[&str] = &[
            $(
                paste! { stringify!( [<$kw:lower>] ) }
            ),+
        ];

        pub const KEYWORD_TYPES: &[Keyword] = &[
            $(
                Keyword:: $kw
            ),+
        ];
    };
}



keywords! {
    As,
    Bool,
    Break,
    Byte,
    Const,
    Continue,
    Double,
    Else,
    Float,
    Fn,
    For,
    If,
    In,
    Int,
    Ref,
    Return,
    String,
    Struct,
    Unt,
}