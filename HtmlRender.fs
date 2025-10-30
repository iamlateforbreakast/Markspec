module HtmlRenderer

    open MarkdownTypes

    let rec private renderSpan (span: Span) : string =
        match span with
        | Text t -> t
        | Bold spans -> sprintf "<strong>%s</strong>" (spans |> List.map renderSpan |> String.concat "")
        | Italic spans -> sprintf "<em>%s</em>" (spans |> List.map renderSpan |> String.concat "")
        | Link (spans, url) -> sprintf "<a href=\"%s\">%s</a>" url (spans |> List.map renderSpan |> String.concat "")
        | Image (alt, url) -> sprintf "<img src=\"%s\" alt=\"%s\" />" url alt
        | Code c -> sprintf "<code>%s</code>" c
        | InlineCode  c -> sprintf "<code>%s</code>" c
    
    let rec renderBlocks (blocks: Block list) : string =
        blocks
        |> List.map (fun block ->
            match block with
            | Heading(level, spans) -> sprintf "<h%d>%s</h%d>" level (spans |> List.map renderSpan |> String.concat "") level
            | Paragraph spans -> sprintf "<p>%s</p>" (spans |> List.map renderSpan |> String.concat "")
            | UnorderedList items ->
                let listItems = items |> List.map (fun blk -> sprintf "<li>%s</li>" (renderBlocks blk)) |> String.concat ""
                sprintf "<ul>%s</ul>" listItems
            | HorizontalRule -> "<hr />"
            | Blockquote subBlocks -> $"""<blockquote>%s{ renderBlocks subBlocks }</blockquote>"""
            | CodeBlock (language, content) ->
                let langAttribute =
                    match language with
                    | Some lang -> sprintf " class=\"language-%s\"" lang
                    | None -> ""
                sprintf "<pre><code%s>%s</code></pre>" langAttribute content
            | _ -> "" // Not implemented yet
            )
        |> String.concat ""
    let render (ast: Document) : string =
        renderBlocks ast
