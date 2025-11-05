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
        | UnorderedList of ListItem list
        | CodeBlock of language: string option * content: string // Added CodeBlock typ
        | OrderedList of ListItem list
        | Blockquote of Block list
        | HorizontalRule
        | RawHtml of string
        | EOF

    // Use Option<int> to store the starting index of an ordered list (1 for 1), None for unordered.
    and ListItem = {
        IndentLevel: int;
        Marker: string; // e.g., "-", "+", "1)", "2)"
        Content: Block list;
        ListType: Option<int>; 
    }

    type Document = Block list

    let printBlock (block : Block) =
        match block with
        | Heading _ -> printfn "Heading"
        | Paragraph _ -> printfn "Paragraph"
        | _ -> printfn "Unknown block"

