module TokenType

    type Lines =
        { Buffer : char list
          Column : int
          Line : int
        }

    type TokenText =
        { Text : string
          Line : int }

    type TokenEOF =
        { Line : int }

    type TokenEOL =
        { Type : int 
          Line : int }

    type Token =
        | Text of TokenText
        | EOF of TokenEOF
        | EOL of TokenEOL

    let isText (c : char) =
        not ((c = '\r') || (c = '\n') || (c = ' ') || (c = '\t'))

    let readTextToken (content : Lines) : Token  * Lines =
        let rec loop(content : Lines)(text: char list) =
            match content.Buffer with
            | [] -> (content, text)
            | c :: rest when isText c -> loop {content with Buffer = rest} (c :: text)
            | _ -> (content, text)
            //chars |> List.toArray |> (fun s -> System.String s)
        let (a,b) = loop content []
        (Text {Text = System.String(b |> List.toArray); Line = content.Line} , a)

    let getToken (content : Lines) : Token * Lines =
        match content.Buffer with
        | [] -> (EOF {Line = content.Line}, { content with Buffer = [] })
        | '\r' :: '\n' :: rest -> ((EOL {Type = 1; Line = content.Line}), { content with Buffer = rest}) // Windows EOL
        | '\n' :: rest -> ((EOL {Type = 2; Line = content.Line}), { content with Buffer = rest}) // Linux EOL
        | '\r' :: rest -> ((EOL {Type = 3; Line = content.Line}), { content with Buffer = rest}) // Mac EOL
        | c :: rest when isText c -> readTextToken content
        | _ -> (Text {Text = "Error"; Line = content.Line}, { content with Buffer = [] }) // Never executed

    let printToken (token : Token) =
        match token with
        | Text t -> printfn "Token: Text { Text = \"%s\"; Line = %d }" t.Text t.Line
        | EOF e -> printfn "Token: EOF { Line = %d }" e.Line
        | EOL e -> printfn "Token: EOL { Type = %d }" e.Type
