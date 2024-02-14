
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
    Match,
    Ref,
    Return,
    String,
    Struct,
    Unt,
}