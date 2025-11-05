// File: Program.fs

module Program

open System
open System.IO
open ParseToken
open MarkdownTypes
open HtmlRenderer

let printUsage() =
    printfn "Usage: markspec <input.md> [output.html]"
    printfn "  input.md     - Path to input Markdown file"
    printfn "  output.html  - Path to output HTML file (optional, defaults to input.md with .html extension)"

// The EntryPoint attribute marks the main function of the application.
[<EntryPoint>]
let main argv =
    match argv with
    | [||] ->
        printUsage()
        1  // Return non-zero to indicate error
    | args ->
        try
            // Get input path from first argument
            let inputPath = Path.GetFullPath(args.[0])
            
            // Default output path: same as input but .html extension
            let outputPath = 
                match args with
                | [| _; outputArg |] -> Path.GetFullPath(outputArg)
                | _ -> Path.ChangeExtension(inputPath, ".html")
                
            // Get header/footer relative to input file directory
            let baseDir = Path.GetDirectoryName(inputPath)
            let htmlHeaderPath = Path.Combine(baseDir, "htmlHeader.html")
            let htmlFooterPath = Path.Combine(baseDir, "htmlFooter.html")
            
            // Read input files
            let markdownText = File.ReadAllText(inputPath)
            let htmlHeaderText = 
                if File.Exists(htmlHeaderPath) then File.ReadAllText(htmlHeaderPath)
                else "<!DOCTYPE html>\n<html>\n<head>\n<title>Markdown Output</title>\n</head>\n<body>\n"
            let htmlFooterText = 
                if File.Exists(htmlFooterPath) then File.ReadAllText(htmlFooterPath)
                else "\n</body>\n</html>"

            // Parse and render
            let parsedDocument = ParseToken.parseDocument markdownText
            let htmlOutput = HtmlRenderer.render parsedDocument
    
            // Write the HTML output
            File.WriteAllText(outputPath, htmlHeaderText)
            File.AppendAllText(outputPath, htmlOutput)
            File.AppendAllText(outputPath, htmlFooterText)
    
            printfn "Successfully converted '%s' to '%s'" inputPath outputPath
            0  // Return 0 for success
        with
        | :? IndexOutOfRangeException ->
            printUsage()
            1
        | :? FileNotFoundException as ex ->
            printfn "Error: Input file not found: %s" ex.FileName
            1
        | :? IOException as ex ->
            printfn "Error accessing file: %s" ex.Message
            1
        | ex ->
            printfn "Error: %s" ex.Message
            1
