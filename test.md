# This is a heading
## This another heading

> This is a single-paragraph blockquote. It's often used to quote someone or to provide a note of special importance.

A paragraph with **bold** and *italic* text. It also contains some `inline code` for a quick command.

> This is a multi-paragraph blockquote.
>
> Blank lines need to have the `>` character to keep the blockquote contiguous.
>
> > Here is a nested blockquote. It is indented with more `>` characters.
> >
> > You can also apply formatting within nested blockquotes, like **bold** text.

This is a paragraph with an inline image. You can see a logo here: ![The logo of a fictional company](https://example.com/images/company-logo.png).

Here is a horizontal rule for visual separation.
---

A paragraph that uses a local image, likely saved in the same directory as the Markdown file. 
![A chart showing quarterly growth](growth_chart.png).

> A blockquote can also contain a list:
>
> * An item inside the blockquote
> * Another item, indented
> * A third item

This is a paragraph that mentions an unordered list below.

* An item
* Another item
* A third item, with an image reference at the end. ![A small thumbnail](thumbnail.jpg).

This is a paragraph that includes a link to an image.
You can view the full-size version of the logo [here](https://example.com/images/company-logo.png).

> Blockquotes can also contain code blocks, which must be indented further.
>
> ```fsharp
> let hello (name: string) : string =
>     sprintf "Hello, %s!" name
>
> hello "World" |> printfn "%s"
> ```
> This is a regular paragraph continuing the blockquote after the code block.

Here is an example of a fenced code block with a language specified, outside of a blockquote.

```fsharp
let myFunction (input: string) : string =
    "Hello, " + input + "!"

printfn "%s" (myFunction "World")
```
