module ParseToken
  open TokenType
  open MarkdownTypes
  open ParseContext

  // Renamed from parseBold to better reflect its function: parsing spans within bold markers
  let rec parseBoldSpans (context : Context) : Span list * Context =
      let rec loop (context : Context)(spans : Span list) : Span list * Context =
          let tokens = context.Tokens
          match tokens with
              // Stop when EOF is reached
              | Token.EOF :: _ -> (spans, context)
              // Stop when a closing bold marker (Level 2 Star) is found
              | Token.Star s :: rest when s.Level = 2 -> (spans, {context with Tokens = rest})
              // Recurse to parse the next span normally within the bold context
              | _ ->
                  let (parsedSpans, newContext) = parseSpan context
                  // Append the newly parsed spans and continue the loop
                  loop newContext (spans @ parsedSpans)

      // Initial call to loop with the starting context and empty span list
      loop context []


  and parseText (context : Context) : Span * Context =
      let rec loop (context : Context)(text : string) : string * Context =
          let tokens = context.Tokens
          match tokens with
              | Token.EOF :: _ -> (text, context)
              | Token.EOL _ :: rest -> (text, {context with Tokens = rest})
              | Token.Text t :: rest -> loop {context with Tokens = rest} (text + t.Text)
              | _ ->
                  // Fallback to getToken if the current token isn't a simple text token
                  let (t, b) = getToken context.Buffer
                  let (l,c) = loop {context with Buffer = b; Tokens = t :: context.Tokens} text
                  (l, c)

      let (s,c) = loop context ""
      (Text s,c)

  and parseLink (context : Context) : Span * Context =
      // First part: parse the link label within brackets: [link label]
      let rec parseLinkLabel (context : Context) (spans : Span list) : Span list * Context =
          match context.Tokens with
          | Token.EOF :: _ -> (spans, context) // Error: link never closed
          | Token.CloseBracket :: rest -> (List.rev spans, {context with Tokens = rest}) // End of label
          | _ -> 
              let (parsedSpans, newContext) = parseSpan context
              parseLinkLabel newContext (spans @ parsedSpans)

      // Second part: parse the URL within parentheses: (url)
      let rec parseLinkUrl (context : Context) (url : string) : string * Context =
          match context.Tokens with
          | Token.EOF :: _ -> (url, context) // Error: link never closed
          | Token.CloseBracket :: rest -> (url, {context with Tokens = rest}) // End of URL
          // Assuming the URL is composed of simple Text tokens or similar that parseText handles
          | _ -> 
              let (Span.Text t, newContext) = parseText context 
              parseLinkUrl newContext (url + t)

      let (labelSpans, contextAfterLabel) = parseLinkLabel context []

      // Expect an open parenthesis immediately after the closing bracket
      match contextAfterLabel.Tokens with
      | Token.OpenBracket :: rest ->
          let (url, contextAfterUrl) = parseLinkUrl {contextAfterLabel with Tokens = rest} ""
          // Assuming a 'Link' case in Span (e.g., Link of Span list * string)
          (Span.Link (labelSpans, url), contextAfterUrl)
      | _ ->
          // If the parenthesis is missing, treat the initial bracketed text as plain text or handle as error
          // For now, return an empty text span and the original context
          (Span.Text "", context)

  and parseSpan (context : Context) : Span list * Context =
      let rec loop (context : Context)(spans : Span list) : Span list * Context =
          let tokens = context.Tokens
          match tokens with
              | Token.EOF :: _ -> (spans, context)
              | Token.EOL _ :: rest -> (spans, {context with Line = context.Line + 1})
              | Token.WhiteSpace _ :: rest ->
                  let (t, b) = getToken context.Buffer
                  let (l,c) = loop {context with Buffer = b; Tokens = t :: rest} spans
                  (l, c)
              | Token.Text t :: rest -> 
                   let (s,c) = parseText context
                   ((s :: spans), c)
              // Handle the opening of a bold span (Level 2 Star)
              | Token.Star s :: rest when s.Level = 2 ->
                  // Parse the spans within the bold section
                  let (boldSpans, c) = parseBoldSpans {context with Tokens = rest}
                  // Create the bold span element and continue looping
                  loop c ((Bold boldSpans) :: spans)
              | _ ->
                  let (t, b) = getToken context.Buffer
                  let (l,c) = loop {context with Buffer = b; Tokens = t :: context.Tokens} spans
                  (l, c)
              //| Token.Italic :: rest ->
              //| Token.Quote q when q.Level = 1 ->

      // Call the loop and reverse the accumulated list at the end to maintain original order
      let (s, c) = loop context []
      (List.rev s, c)
  (* 
  Parses the content *after* the list marker. 
  This is tricky as list content can span multiple lines and contain other blocks.
  For simplicity here, we assume content is just a single paragraph (Span list), 
  but your types allow for Block list content. A full implementation would need a 
  recursive call to parseBlock for indented lines.
  We will use parseSpan for simple content here.
  *)
  let parseListItemContent (context : Context) : Span list * Context =
   // For a simple single-line list item, parseSpan is sufficient
   parseSpan context

  (* 
     New function to parse ordered lists.
     It expects the current token to be an OrderedListItem and continues 
     parsing subsequent items until a non-list item or EOF is reached.
  *)
  let rec parseOrderedList (context : Context) : Block * Context =
      let rec loop (context : Context) (items : ListItem list) : ListItem list * Context =
          // Ensure tokens are populated for lookahead
          let currentContext =
              if (List.isEmpty context.Tokens && not (List.isEmpty context.Buffer)) then
                  let (t, b) = getToken context.Buffer
                  {context with Buffer = b; Tokens = t :: context.Tokens}
              else
                  context

          match currentContext.Tokens with
          // Check for the next item marker
          | Token.OrderedListItem info :: rest -> 
              let level = info.Level // Extract the actual number/level

              let contextAfterMarker = {currentContext with Tokens = rest}
              let (contentSpans, contextAfterContent) = parseListItemContent contextAfterMarker

              // Wrap the spans in a Paragraph block (assuming contentSpans type is List<Span>)
              let contentBlocks = 
                  if List.isEmpty contentSpans then []
                  else [Paragraph contentSpans]

              let newItem: ListItem = {
                  IndentLevel = 0; // You don't parse indent level in getToken yet
                  Marker = string level; // Use the level as the marker text
                  Content = contentBlocks; 
                  ListType = Some level; // Use the parsed number as start index
              }
              // Recurse to find the next item in the sequence
              loop contextAfterContent (items @ [newItem])
          | _ ->
              // Stop when a non-list item token is encountered
              (items, currentContext)

      // Start the loop and wrap the result in the Block.OrderedList type
      let (items, finalContext) = loop context []
      (Block.OrderedList items, finalContext)
      
  let rec parseBlock (inputBuffer : string) : Document =
      let rec loop (context : Context) : Context =
          // Ensure tokens are populated
          let currentContext =
              if (List.isEmpty context.Tokens && not (List.isEmpty context.Buffer)) then
                  let (t, b) = getToken context.Buffer
                  {context with Buffer = b; Tokens = t :: context.Tokens}
              else
                  context
          match currentContext.Tokens with
             | Token.EOF :: _ -> 
                  {currentContext with Blocks = (Block.EOF :: currentContext.Blocks); Tokens = []}
             | Token.EOL _ :: rest -> 
                  loop {currentContext with Tokens = rest; Line = currentContext.Line + 1; Column = 1}
             | Token.Hash h :: rest when (h.Level <= 6) -> 
                  let (l,c) = parseSpan {currentContext with Tokens = rest}
                  let nh = (Heading (h.Level,  l))
                  let nb = (nh :: currentContext.Blocks)
                  loop {c with Blocks = nb}
             | Token.OrderedListItem _ :: _ ->
                 let (olBlock, c) = parseOrderedList currentContext
                 loop {c with Blocks = olBlock :: c.Blocks}
             | _ -> 
                  // Example handling for text as paragraph
                  let (spans, c) = parseSpan currentContext
                  if List.isEmpty spans then // If parseSpan returned nothing useful, keep looping to next token
                      loop c
                  else
                      loop {c with Blocks = Paragraph spans :: c.Blocks}
                  // CodeBlocks
                  // HorizontalRule
                  // UnorderedList
                  // OrderedList
                  // BlockQuote
                  // RawHtml


      let c = {Buffer = (inputBuffer |> Seq.toList); Line = 1; Column = 1; Spans = []; Tokens = []; Blocks = []}
      let d = loop c
      d.Blocks |> List.rev
