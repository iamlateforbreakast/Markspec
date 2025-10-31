module TokenType

    type TokenText =
        { Text : string
          Line : int }

    type TokenEOF =
        { Line : int }

    type Token =
        | Text of TokenText
        | EOF of TokenEOF

    let getToken (content : char list)(line: int) : Token * char list =
        match content with
        | [] -> (EOF {Line = line}, [])
        | ' ' :: rest -> (Text {Text = "Hello"; Line = line} , [])
        | _ -> (Text {Text = "Hello"; Line = line} , [])
