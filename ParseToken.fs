module ParseToken
    open TokenType
    open MarkdownTypes

    let rec parseBold (tokens : Token list)(spans : Span list) : Span list =
        let rec loop (tokens : Token list)(spans : Span list) =
            match tokens with
                | Token.EOF :: _ -> spans
                | Token.Star s :: _ when (s.Level =1) -> spans
                | Token.Text t :: remainingTokens -> loop remainingTokens (Span.Text t.Text :: spans)
                | _ -> []

        loop tokens spans

    let rec parseSpan (tokens : Token list)(spans : Span list) : Span list =
        let rec loop (tokens : Token list)(spans : Span list) =
            match tokens with
                | Token.EOF :: _ -> spans
                | Token.Text t :: remainingTokens -> loop remainingTokens (Span.Text t.Text :: spans)
                | Token.Star s1 :: remainingTokens when (s1.Level = 1) -> (Span.Bold (parseBold remainingTokens []) :: spans)
                | _ -> []

        loop tokens []

    let rec parseBlock (tokens : Token list) : Block option =
        let a = (tokens |> List.rev)
        match a with
            | Token.EOF :: remainingTokens -> Some(Block.EOF)
            | Token.Hash _ :: Token.WhiteSpace _ :: remainingTokens -> 
                Some(Block.Heading (0, parseSpan remainingTokens []))
            | Token.Text _ :: remainingTokens -> Some(Paragraph [])
            | _ -> Option.None

    let parseDocument (inputBuffer : string) : Document =
        let rec loop (content : Lines) (tokens : Token list)(blocks : Block list) : Block list =
            let (nextToken, remainingChars) = getToken content.Buffer
            printToken nextToken
            match nextToken with
            | Token.EOF -> blocks
            | Token.EOL _ -> 
                let nextBlock = parseBlock tokens
                match nextBlock with
                    | None -> loop {content with Buffer = remainingChars} tokens blocks
                    | Some b when b = EOF -> blocks
                    | Some b -> loop {content with Buffer = remainingChars} [] (b::blocks)
            | _ ->
                let nextBlock = parseBlock (nextToken::tokens)
                match nextBlock with
                    | None -> loop {content with Buffer = remainingChars} (nextToken::tokens) blocks
                    | Some b when b = EOF -> blocks
                    | Some b -> loop {content with Buffer = remainingChars} [] (b::blocks)

        let lines = {Buffer = (inputBuffer |> Seq.toList); Line = 1; Column = 1}
        let blocks = []
        ((loop lines [] []) |> List.rev)
