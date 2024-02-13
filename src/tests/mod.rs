use std::{error::Error};

use crate::parser::token::{TokenStream, TokenType};

pub mod test_lexer;
 


pub trait Matcher<T> {
    fn assert_matches(&self, expected: T) -> Result<(), String>;
}

impl<'src> Matcher<&[(&str, TokenType)]> for TokenStream<'src> {
    fn assert_matches(&self, expected: &[(&str, TokenType)]) -> Result<(), String> {
        if expected.len() != self.len() {
            return Err(format!(
                "Expected {:?} but got {:?}",
                expected.iter().map(|t| t.0).collect::<Vec<_>>(),
                self.iter().map(|t| t.contents()).collect::<Vec<_>>()
            ));
        }
        for (actual, e) in self.iter().zip(expected.iter()) {
            if actual.token_type() != e.1 {
                return Err(format!(
                    "Expected {:?} but got {:?}",
                    expected.iter().map(|t| t.1).collect::<Vec<_>>(),
                    self.iter().map(|t| t.token_type()).collect::<Vec<_>>()
                ));
            }
            if actual.contents() != e.0 {
                return Err(format!(
                    "Expected {:?} but got {:?}",
                    expected.iter().map(|t| t.0).collect::<Vec<_>>(),
                    self.iter().map(|t| t.contents()).collect::<Vec<_>>()
                ));
            }
        }
        Ok(())
    }
}