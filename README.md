Run `cargo doc --open` for documentation

In order to run, use the following commands:

To run the lexer: 
`cargo run --bin lexer -- --input "The string you want to lexify" `

To run the parser
`cargo run --bin parser -- <output> --input "The string you want to parse"`

Where `output` is one of `rust`, `json`, `yml`

To run the tac generator 
`cargo run --bin tac -- --input "The string you want to parse"`

To run the HLA generator 
`cargo run --bin hla -- --input "The string you want to parse"`

