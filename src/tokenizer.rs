pub mod types;

use types::TokenType;

struct Token {
    token_type: TokenType,
    value: String,
    line: u32,
    column: u32,
}
