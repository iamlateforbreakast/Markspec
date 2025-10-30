module Parser

    open System
    open MarkdownTypes

    let rec parseSpans (text: string) : Span list =
            let rec loop (charList: char list) (spans: Span list) =
                match charList with
                // **Bold**
                |   '*' :: '*' :: rest ->
                    let boldText, restOfLine = 
                        let (before, after) = 
                            let s = new string(rest |> Array.ofList)
                            let index = s.IndexOf "**"
                            if index >= 0 then
                                (s.Substring(0, index), s.Substring(index + 2))
                            else
                                (s, "")
                        (before, after |> Seq.toList)
                    if not (String.IsNullOrEmpty(boldText)) then
                       let innerSpans = parseSpans boldText
                       loop restOfLine (Bold(innerSpans) :: spans)
                    else // Malformed bold, treat as text
                       loop ('*' :: '*' :: rest) spans
                
                // _Italic_
                | '_' :: rest ->
                    let italicText, restOfLine =
                        let (before, after) = 
                            let s = new string(rest |> Array.ofList)
                            let index = s.IndexOf "_"
                            if index >= 0 then
                                (s.Substring(0, index), s.Substring(index + 1))
                            else
                                (s, "")
                        (before, after |> Seq.toList)
                    if not (String.IsNullOrEmpty italicText) then
                        let innerSpans = parseSpans italicText
                        loop restOfLine (Italic(innerSpans) :: spans)
                    else // Malformed italic, treat as text
                        loop ('_' :: rest) spans

                // Image: ![alt](url)
                | '!' :: '[' :: rest ->
                    let s = new string(rest |> Array.ofList)
                    let altEndIndex = s.IndexOf "]"
                    if altEndIndex >= 0 then
                         let altText = s.Substring(0, altEndIndex)
                         let afterAlt = s.Substring(altEndIndex + 1)
                         if afterAlt.StartsWith "(" then
                             let urlStartIndex = afterAlt.IndexOf "("
                             let urlEndIndex = afterAlt.IndexOf ")"
                             if urlEndIndex >= 0 then
                                 let url = afterAlt.Substring(urlStartIndex + 1, urlEndIndex - urlStartIndex - 1)
                                 let restOfLine = afterAlt.Substring(urlEndIndex + 1) |> Seq.toList
                                 loop restOfLine (Image(altText, url) :: spans)
                             else // Malformed image, treat as text
                                 loop ('!' :: '[' :: rest) spans
                         else // Malformed image, treat as text
                             loop ('!' :: '[' :: rest) spans
                    else // Malformed image, treat as text
                         loop ('!' :: '[' :: rest) spans

                // Link: [text](url)
                | '[' :: rest ->
                     let s = new string(rest |> Array.ofList)
                     let linkTextEndIndex = s.IndexOf "]"
                     if linkTextEndIndex >= 0 then
                        let linkText = s.Substring(0, linkTextEndIndex)
                        let afterLinkText = s.Substring(linkTextEndIndex + 1)
                        if afterLinkText.StartsWith "(" then
                           let urlStartIndex = afterLinkText.IndexOf "("
                           let urlEndIndex = afterLinkText.IndexOf ")"
                           if urlEndIndex >= 0 then
                              let url = afterLinkText.Substring(urlStartIndex + 1, urlEndIndex - urlStartIndex - 1)
                              let restOfLine = afterLinkText.Substring(urlEndIndex + 1) |> Seq.toList
                              let innerSpans = parseSpans linkText
                              loop restOfLine (Link(innerSpans, url) :: spans)
                           else // Malformed link, treat as text
                              loop ('[' :: rest) spans
                        else // Malformed link, treat as text
                           loop ('[' :: rest) spans
                      else // Malformed link, treat as text
                     loop ('[' :: rest) spans

                // Inline code: `code`
                | '`' :: rest ->
                    let s = new string(rest |> Array.ofList)
                    let codeEndIndex = s.IndexOf "`"
                    if codeEndIndex >= 0 then
                        let codeContent = s.Substring(0, codeEndIndex)
                        let restOfLine = s.Substring(codeEndIndex + 1) |> Seq.toList
                        loop restOfLine (InlineCode(codeContent) :: spans)
                    else
                        loop ('`' :: rest) spans

                // Plain text
                | c :: rest ->
                    let rec consumeText acc (chars: char list) =
                        match chars with
                        | [] -> (acc, [])
                        | '*' :: '*' :: _ | '_' :: _ | '`' :: _ | '[' :: _ | '!' :: '[' :: _ -> (acc, chars)
                        | ch :: rest -> consumeText (acc + string ch) rest
                    let text, restOfLine = consumeText (string c) rest
                    loop restOfLine (Text(text) :: spans)
                
                // End of line
                | [] -> spans |> List.rev

            loop (text |> Seq.toList) []

    // Helper function to parse table cells
    let private parseCells (line: string) : Span list list =
        line.Trim('|')
        |> (fun s -> s.Split([|'|'|], StringSplitOptions.None))
        |> Array.toList
        |> List.map (fun cell -> parseSpans (cell.Trim()))

    // Helper to determine column alignment
    let private parseAlignment (alignLine: string) : Alignment list =
        let alignCells = 
            alignLine.Trim('|')
            |> (fun s -> s.Split([|'|'|], StringSplitOptions.None))
            |> Array.toList

        alignCells
        |> List.map (fun cell ->
            let trimmed = cell.Trim()
            if trimmed.StartsWith(":") && trimmed.EndsWith(":") then Center
            else if trimmed.EndsWith(":") then Right
            else Left)

    // A dedicated function to parse a complete table
    //let rec private parseTable (lines: string array) (lineIndex: int) (header: Span list list) (alignments: Alignment list) : Table * int =
    //    let rec consumeRows (rows: Span list list) (rowLineIndex: int) =
    //        if rowLineIndex >= lines.Length then
    //            (rows |> List.rev, rowLineIndex)
    //        else
    //            let currentRowLine = lines.[rowLineIndex].Trim()
    //            if currentRowLine.Contains "|" then
    //                let rowCells = parseCells currentRowLine
    //                if not (rowCells |> List.isEmpty) && ((List.length rowCells) = (List.length header)) then
    //                    consumeRows (rowCells :: rows) (rowLineIndex + 1)
    //                else
    //                    (rows |> List.rev, rowLineIndex)
    //            else
    //                (rows |> List.rev, rowLineIndex)

    //    let rows, nextIndex = consumeRows [] lineIndex
    //    let tableBlock = { Header = header; Alignment = alignments; Rows = rows }
    //    (tableBlock, nextIndex)

    let private findBlockquoteLines (lines: string array) (startIndex: int) =
        let rec collectQuotes i acc =
            if i < lines.Length && lines.[i].Trim().StartsWith ">" then
                collectQuotes (i + 1) (lines.[i] :: acc)
            else
                (List.rev acc |> Array.ofList, i)
        collectQuotes startIndex []

    // Simple line-based block parser (highly simplified)
    let rec parseBlocks (lines: string array) : Block list =
        let rec loop blocks (lineIndex: int) =
            if lineIndex >= lines.Length then
                blocks |> List.rev
            else
                let line = lines.[lineIndex]
                let trimmedLine = line.Trim()
                    
                printfn "=>%s" trimmedLine

                match trimmedLine with
                | trimmed when trimmed.StartsWith "```" ->
                    let language =
                        let parts = trimmed.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                        if parts.Length > 1 then Some parts.[1] else None

                    // Find the end of the code block
                    let endLineIndex =
                        lines
                        |> Array.skip (lineIndex + 1)
                        |> Array.tryFindIndex (fun l -> l.Trim().StartsWith "```" )

                    match endLineIndex with
                    | Some i ->
                        let codeLines = lines |> Array.skip (lineIndex + 1) |> Array.take i
                        let codeContent = String.concat Environment.NewLine codeLines
                        let codeBlock = CodeBlock(language, codeContent) // Assuming CodeBlock can take language
                        loop (codeBlock :: blocks) (lineIndex + i + 2) // +2 to move past both fence lines
                    | None -> // No closing fence found, treat as regular paragraph
                        let paraSpans = parseSpans trimmedLine
                        loop (Paragraph(paraSpans) :: blocks) (lineIndex + 1)

                | trimmed when trimmed.StartsWith "#" ->
                    let headingChars = Seq.takeWhile (fun c -> c = '#') trimmed
                    let level = Seq.length headingChars
                    let headingText = trimmed.Substring(level).Trim()
                    let headingSpans = parseSpans headingText
                    loop (Heading(level, headingSpans) :: blocks) (lineIndex + 1)
                    
                | trimmed when trimmed.StartsWith "---" && trimmed.Length = 3 ->
                    loop (HorizontalRule :: blocks) (lineIndex + 1)
                    
                | trimmed when trimmed.StartsWith "*" || trimmed.StartsWith "-" ->
                    let rec parseListItems currentItems currentIndex =
                        if currentIndex >= lines.Length then
                            (currentItems |> List.rev, currentIndex)
                        else
                            let currentLine = lines.[currentIndex].Trim()
                            if currentLine.StartsWith "*" || currentLine.StartsWith "-" then
                                let itemText = currentLine.Substring(1).Trim()
                                let itemSpans = parseSpans itemText
                                parseListItems (Paragraph(itemSpans) :: currentItems) (currentIndex + 1)
                            else
                                (currentItems |> List.rev, currentIndex)

                    let listItems, nextIndex = parseListItems [] lineIndex
                    loop (UnorderedList [listItems] :: blocks) nextIndex

                | trimmed when trimmed.StartsWith ">" ->
                     // Group all consecutive blockquote lines
                     let (quoteLines, nextIndex) = findBlockquoteLines lines lineIndex

                     // Extract content by removing the ">" prefix and recursively parse
                     let contentLines =
                         quoteLines
                         |> Array.map (fun l -> l.TrimStart([|'>';' '|]))

                     let blockquoteContent = parseBlocks contentLines
                     let newIndex = lineIndex + Array.length quoteLines
                     
                     loop (Blockquote(blockquoteContent) :: blocks) newIndex

                | "" ->
                    loop blocks (lineIndex + 1)

                | _ ->
                    let paraSpans = parseSpans trimmedLine
                    loop (Paragraph(paraSpans) :: blocks) (lineIndex + 1)
 
        let parsedBlocks = loop [] 0
        parsedBlocks

    /// Main function to parse a string into a Markdown Document AST
    let parse (markdownText: string) : Document =
        // Specify an array with multiple line endings to be more robust
        let lineSeparators = [| "\r\n"; "\n" |]
        let lines = markdownText.Split(lineSeparators, StringSplitOptions.None)
        parseBlocks lines
