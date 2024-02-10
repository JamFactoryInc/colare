## Lexing
Produce a TokenStream
Tokens contain a string slice, line number, column start pos, and a token type

## Parsing
Parse the tokens into a ParseStream
A ParseStream is a linear stream of ParseTokens, which are either directives such as BeginStatement(StatementType), or are a Leaf that contain a Token

## Identifier resolution


## Semantic analysis

