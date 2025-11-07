module ParseToken
    open TokenType
    open MarkdownTypes
    open ParseContext

    let rec parseBold (tokens : Token list)(spans : Span list) : Span list =
        let rec loop (tokens : Token list)(spans : Span list) =
            match tokens with
                | Token.EOF :: _ -> spans
                | Token.Star s :: _ when (s.Level =1) -> spans
                | Token.Text t :: remainingTokens -> loop remainingTokens (Span.Text t.Text :: spans)
                | _ -> []

        loop tokens spans

    let parseText (context : Context) : Span * Context =
        let rec loop (context : Context)(text : string) : string * Context =
            let tokens = context.Tokens
            match tokens with
                | Token.EOF :: _ -> (text, context)
                | Token.EOL _ :: rest -> (text, {context with Tokens = rest})
                | Token.Text t :: rest -> loop {context with Tokens = rest} (text + t.Text)
                | _ ->
                    let (t, b) = getToken context.Buffer
                    let (l,c) = loop {context with Buffer = b; Tokens = t :: context.Tokens} text
                    (l, c)

        let (s,c) = loop context ""
        (Text s,c)

    let parseSpan (context : Context) : Span list * Context =
        let rec loop (context : Context)(spans : Span list) : Span list * Context =
            let tokens = context.Tokens
            match tokens with
                | Token.EOF :: _ -> (spans, context)
                | Token.EOL _ :: rest -> (spans, {context with Line = context.Line + 1})
                | Token.WhiteSpace _ :: rest ->
                    let (t, b) = getToken context.Buffer
                    let (l,c) = loop {context with Buffer = b; Tokens = t :: context.Tokens} spans
                    (l, c)
                | Token.Text t :: rest -> 
                     let (s,c) = parseText context
                     ((s :: spans), c)
                | _ ->
                    let (t, b) = getToken context.Buffer
                    let (l,c) = loop {context with Buffer = b; Tokens = t :: context.Tokens} spans
                    (l, c)
                //| Token.Bold :: rest ->
                //| Token.Italic :: rest ->
                //| Token.Quote q when q.Level = 1 ->

        loop context []

    let rec parseBlock (inputBuffer : string) : Document =
        let rec loop (context : Context) : Context =
           let a = (context.Tokens |> List.rev)
           let b = (context.Blocks |> List.rev)
           match a with
              | Token.EOF :: rest -> {context with Blocks = Block.EOF :: b; Tokens = rest}
              | Token.EOL _ :: rest -> {context with Line = context.Line + 1; Column = 1}
              | Token.Hash h :: rest when (h.Level <= 6) -> 
                    let (l,c) = parseSpan {context with Tokens = rest}
                    let b = Heading (h.Level, l)
                    {c with Blocks = b :: context.Blocks} 
              | _ -> 
                let (t, b) = getToken context.Buffer
                loop {context with Buffer = b; Tokens = t :: context.Tokens}
              // CodeBlocks
              // HorizontalRule
              // UnorderedList
              // OrderedList
              // BlockQuote
              // RawHtml


        let c = {Buffer = (inputBuffer |> Seq.toList); Line = 1; Column = 1; Spans = []; Tokens = []; Blocks = []}
        let d = loop c
        d.Blocks
