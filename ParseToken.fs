module ParseToken
    open TokenType

    let printToken (tokens : Token list) =
        (tokens |> List.iter printToken)

    let parseToken (content : string) : Token list =
        let rec loop (rest : Lines) (tokens : Token list) : Token list =
            let (newToken,rest) = getToken rest
            match newToken with
            | EOF _ -> (newToken :: tokens)
            | EOL _ -> loop {rest with Line = rest.Line + 1} tokens
            | _ -> loop rest (newToken::tokens) 

        let lines = {Buffer = (content |> Seq.toList); Line = 0; Column = 0}
        loop lines [] |> List.rev
