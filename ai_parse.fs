namespace Tokenizer

open TokenType

module ParseToken =
    /// Converts a string of content into a list of Tokens.
    let parseToken (content : string) : Token list =
        // The recursive function that does the work.
        // rest: The remaining characters to process.
        // tokens: The accumulator list of Tokens (built in reverse order).
        let rec loop (rest : char list) (tokens : Token list) =
            // 1. Get the next token and the remaining list of characters
            let (newToken, nextRest) = TokenType.getToken rest 0

            // 2. Pattern match on the new token
            match newToken with
            | TokenEOF ->
                // Base Case: We hit the end of the input.
                // We must reverse the accumulator to get the correct order.
                List.rev (TokenEOF :: tokens)
            | _ ->
                // Recursive Step: Continue the loop with the remaining characters
                // and add the new token to the front of the accumulator list.
                loop nextRest (newToken :: tokens)

        // Start the process by converting the input string to a character list
        // and initializing the accumulator list to empty.
        loop (content |> Seq.toList) []

// --- Example Usage ---

module Main =
    open System
    [<EntryPoint>]
    let main argv =
        let input = "a=1+b"
        printfn "Tokenizing input: \"%s\"" input

        let tokens = ParseToken.parseToken input

        tokens
        |> List.iter (fun token -> printfn "  -> %A" token)

        0 // return an integer exit code