module MarkdownTypes
    // Table cell alignment
    type Alignment =
       | Left
       | Right
       | Center
       | None_

    // Inline elements within a block
    type Span =
        | Text of string
        | Bold of Span list
        | Italic of Span list
        | Code of string
        | Image of altText: string * url: string
        | Link of text: Span list * url: string
        | InlineCode of string

    // Table elements
    type Table = 
        { Header: Span list list
          Alignment: Alignment list
          Rows: Span list list }

    // Block-level elements
    type Block =
        | Paragraph of Span list
        | Heading of int * Span list // `int` for the heading level (1-6)
        | UnorderedList of Block list list
        | CodeBlock of language: string option * content: string // Added CodeBlock typ
        | OrderedList of Block list list
        | Blockquote of Block list
        | HorizontalRule
        | RawHtml of string

    type Document = Block list

