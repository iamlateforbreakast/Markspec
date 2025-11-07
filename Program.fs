// File: Program.fs

module Program

open System
open System.IO
open ParseToken
open MarkdownTypes
open HtmlRenderer

// The EntryPoint attribute marks the main function of the application.
[<EntryPoint>]
let main argv =
    // The markdown content to be parsed.
    let filePath = @"..\\..\\..\\test.md"
    let outputFilePath = @"..\\..\\..\\output.html"
    let htmlHeaderPath = @"..\\..\\..\\htmlHeader.html"
    let htmlFooterPath = @"..\\..\\..\\htmlFooter.html"
    try
        // Read the entire file content into a string
        let markdownText = File.ReadAllText(filePath)
        let htmlHeaderText = File.ReadAllText(htmlHeaderPath)
        let htmlFooterText = File.ReadAllText(htmlFooterPath)

        // Pass the string to your parser
        let parsedDocument = ParseToken.parseBlock markdownText

        (parsedDocument |> List.iter printBlock)
        //let parsedDocument = Parser.parse markdownText
        let htmlOutput = HtmlRenderer.render parsedDocument
    
        // Write the HTML string to a file
        File.WriteAllText(outputFilePath, htmlHeaderText)
        File.AppendAllText(outputFilePath, htmlOutput)
        File.AppendAllText(outputFilePath, htmlFooterText)
    
        printfn "Markdown parsed and saved to %s" outputFilePath
    with
    | :? FileNotFoundException ->
        printfn $"Error: The file '{filePath}' was not found."
    | ex ->
        printfn $"An unexpected error occurred: %s{ex.Message}"
        printfn $"Exception type: %A{ex.GetType()}" // Use GetType() to see the exact type
    // Return 0 to indicate successful execution.
    0
