module ParseToken
    open TokenType

    let parseToken (content : string) : Token list =
        let rec loop (rest : char list) (tokens : Token list) =
            let (newToken,rest) = getToken rest 0
            match newToken with
            | TokenEOF -> TokenEOF :: tokens
            | _ -> loop ()
        let tokens = []
        let (newToken,rest) = getToken (content |> Seq.toList) 0
        newToken :: tokens
