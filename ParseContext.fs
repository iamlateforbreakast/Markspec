module ParseContext
    open TokenType
    open MarkdownTypes

    type Context =
        { Buffer : char list
          Line : int
          Column : int
          Tokens : Token list
          Blocks : Block list
          Spans : Span list
        }
