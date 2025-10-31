namespace Tokenizer

module TokenType =
    // 1. Define the Tokens using a Discriminated Union (DU)
    type Token =
        | TokenIdentifier of string
        | TokenNumber of int
        | TokenEquals
        | TokenPlus
        | TokenEOF
        | TokenUnknown of string

    // 2. Mock 'getToken' function to simulate token extraction.
    // In a real tokenizer, this would read from 'rest' and determine the next token.
    // We simulate by consuming the first character to ensure the recursion terminates.
    let getToken (rest : char list) (pos : int) : Token * char list =
        match rest with
        | [] -> (TokenEOF, [])
        | head :: tail when Char.IsDigit head -> (TokenNumber (int (string head)), tail)
        | head :: tail when Char.IsLetter head -> (TokenIdentifier (string head), tail)
        | '=' :: tail -> (TokenEquals, tail)
        | '+' :: tail -> (TokenPlus, tail)
        | head :: tail -> (TokenUnknown (string head), tail)