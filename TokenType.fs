module TokenType

    type Lines =
        { Buffer : char list
          Line : int
          Column : int
        }

    type TokenText =
        { Text : string
        }

    type TokenHash =
        { Level : int
        }

    type TokenStar =
        { Level : int
        }

    type TokenWhiteSpace =
        { Level : int
        }

    type TokenBlockQuote =
        { Level : int
        }

    type TokenQuote =
        { Level : int
        }

    type TokenEOL =
        { Type : int 
        }

    type Token =
        | Text of TokenText
        | Hash of TokenHash
        | Star of TokenStar
        | Quote of TokenQuote
        | WhiteSpace of TokenWhiteSpace
        | Exclamation
        | OpenSquareBracket
        | CloseSquareBracket
        | OpenBracket
        | CloseBracket
        | EOL of TokenEOL
        | EOF
       
    let isText (c : char) =
        not ((c = '\r') || (c = '\n') || (c = ' ') || (c = '\t') || (c = '*'))

    let isWhiteSpace (c : char) =
        ((c = ' ') || (c = '\t'))

    let readTextToken (content : char list) : Token  * char list =
        let rec loop(content : char list)(text: char list) =
            match content with
            | [] -> (content, text)
            | c :: rest when isText c -> loop rest (c :: text)
            | '*' :: rest -> (content, text)
            | ' ' :: c :: rest when isText c -> loop rest (c :: ' ' :: text)
            | ' ' :: c :: rest -> ((c :: rest),(' ' :: text))
            | _ -> (content, text)
            //chars |> List.toArray |> (fun s -> System.String s)
        let (a,b) = loop content []
        (Text {Text = System.String(b |> List.rev |> List.toArray)} , a)

    let processStar (content : char list) : Token * char list =
        let rec loop(content : char list)(length : int) =
            match content with
            | '*' :: remainingChars -> loop remainingChars (length + 1)
            | ' ' :: remainingChars -> (remainingChars, (length + 1))
            | _ -> (content, length)
        let (a,b) = loop content 0
        (Star {Level = b}, a)

    let processHash (content : char list) : Token * char list =
        let rec loop(content : char list)(length : int) =
            match content with
            | '#' :: remainingChars -> loop remainingChars (length + 1)
            | _ -> (content, length)
        let (a,b) = loop content 0
        (Hash {Level = b}, a) 

    let processWhiteSpace (content : char list) : Token * char list =
        let rec loop(content : char list)(length : int) =
            match content with
            | ' ' :: remainingChars -> loop remainingChars (length + 1)
            | '\t' :: remainingChars -> loop remainingChars (length + 1)
            | _ -> (content, length)
        let (a,b) = loop content 0
        (WhiteSpace {Level = b}, a) 

    let processQuote (content : char list) : Token * char list =
        let rec loop(content : char list)(length : int) =
            match content with
            | '\'' :: remainingChars -> loop remainingChars (length + 1)
            | _ -> (content, length)
        let (a,b) = loop content 0
        (Quote {Level = b}, a) 

    let getToken (content : char list) : Token * char list =
        match content with
        | [] -> (EOF, [] )
        | '\r' :: '\n' :: rest -> (EOL {Type = 1}, rest) // Windows EOL
        | '\n' :: rest -> (EOL {Type = 2}, rest) // Linux EOL
        | '\r' :: rest -> (EOL {Type = 3}, rest) // Mac EOL
        | '*' :: rest -> processStar content
        | '#' :: rest -> processHash content
        | '\'' :: rest -> processQuote content
        | ' ' :: rest -> processWhiteSpace content
        | _ -> readTextToken content  // Handle any other character as text

    /// Debug helper to print token information
    let printToken (token : Token) =
        match token with
        | Text t -> printfn "Token: Text { Text = \"%s\"}" t.Text
        | Star s -> printfn "Token: Star { Length = %d }" s.Level
        | Hash h -> printfn "Token: Hash { Length = %d }" h.Level
        | WhiteSpace w -> printfn "Token: WhiteSpace { Length = %d }" w.Level
        | Quote q -> printfn "Token: Quote { Length = %d }" q.Level
        | OpenSquareBracket -> printfn "Token: Open Square Bracket"
        | CloseSquareBracket -> printfn "Token: Close Square Bracket"
        | OpenBracket -> printfn "Token: Open Bracket"
        | CloseBracket -> printf "Token: Close Bracket"
        | Exclamation -> printfn "Token: Exclamation"
        | EOF -> printfn "Token: EOF"
        | EOL e -> printfn "Token: EOL { Type = %d }" e.Type
