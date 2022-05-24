module FML.Tests.ParserCombinators2

open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Core

module TextInput =
    type Position =
        {
            line : int
            column : int
        }
    
    /// define an initial position
    let initialPos = {line=0; column=0}
    
    let incrCol (pos:Position) =
        {pos with column=pos.column + 1}

    /// increment the line number and set the column to 0
    let incrLine pos =
        {line=pos.line + 1; column=0}

    type InputState =
        {
            Position : Position
            Iterator: IEnumerator<Rune>
        }

    let nextRune input =
        let hasRune = input.Iterator.MoveNext()
        
        if hasRune then
            let newPos =
                if input.Iterator.Current <> Rune('\n') then
                    incrCol input.Position
                else
                    incrLine input.Position
            let newState = {input with Position=newPos}
            newState, Some input.Iterator.Current
        else
            input, None

let normalizeLineEndings (runes: Rune seq) =
    seq {
        use i = runes.GetEnumerator()
        let returnRune = Rune('\r')
        let newlineRune = Rune('\n')
        let mutable prevR = false
                
        while i.MoveNext() do
            if i.Current = returnRune then
                prevR <- true
                yield newlineRune
            elif i.Current = newlineRune && prevR then
                prevR <- false
            else
                prevR <- false
                yield i.Current
    }

type Position =
    {
        line : int
        column : int
    }

let incrCol (pos:Position) =
    {pos with column=pos.column + 1}

/// increment the line number and set the column to 0
let incrLine pos =
    {line=pos.line + 1; column=0}

type Token =
    | XmlDecl

let parseXml (runes: Rune seq) =
    seq {
        use i = runes.GetEnumerator()
        let k = 5
        let mutable position = {line=1; column=0}
        let mutable p = 0
        
        let lookahead: Rune option array = Array.create k None
        
        let consume () =
            let next =
                if i.MoveNext()
                then
                    position <-
                        if i.Current <> Rune('\n')
                        then incrCol position
                        else incrLine position
                    
                    Some i.Current
                else None
            
            lookahead.[p] <- next
            p <- (p+1) % k
        
        [1..k]
        |> List.iter (fun _ -> consume())
        
        let la i =
           lookahead.[(p+i) % k] 
        
        let match' rune =
            if rune = la 0
            then
                consume()
            else
                failwith $"Expecting {string rune}; found {string (la 0)}"
        
//        let (|Literal|_|) (c: char) rune =
//            if (Rune c) = rune
//            then Some rune
//            else None
//        
//        let (|InCharList|_|) (list: char list) (rune: Rune) =
//            let runes =
//                list
//                |> List.map Rune
//            
//            if List.contains rune runes
//            then Some rune
//            else None
//        
//        let (|InRange|_|) s e (rune: Rune) =
//            if rune.Value >= s && rune.Value <= e
//            then Some rune
//            else None
//        
//        let isNameStartChar (r: Rune) =
//            let capitalAtoZ = ['A'..'Z']
//            let lowercaseAtoZ = ['a'..'z']
//            
//            match r with
//            | Literal ':' _
//            | InCharList capitalAtoZ _
//            | Literal '_' _
//            | InCharList lowercaseAtoZ _
//            | InRange 0xC0 0xD6 _ -> true
//        
//        let xmlDecl () =
//            ()
//        
//        let misc () =
//            match "" with
//            | "<!--" -> () // comment
//            | "<?" -> () // processing instruction
//            | 0x20 | 0x9 | 0xD | 0xA -> () // whitespace
        
        let isXmlDecl () =
            la 0 = Some (Rune '<') &&
            la 1 = Some (Rune '?') &&
            la 2 = Some (Rune 'x') &&
            la 3 = Some (Rune 'm') &&
            la 4 = Some (Rune 'l')
        
        let matchXmlDecl () =
            match' (Some (Rune '<'))
            match' (Some (Rune '?'))
            match' (Some (Rune 'x'))
            match' (Some (Rune 'm'))
            match' (Some (Rune 'l')) 
            XmlDecl
        
        let element () =
            seq {
                XmlDecl
            }
        
        let isMisc() = false
        
        let misc() = XmlDecl
        
        let prolog () =
            seq {
                if isXmlDecl() then yield matchXmlDecl()
            }
                
                
            
//            match "" with
//            | "<?xml" -> () // XMLDecl
//            | "<!DOCTYPE" -> () // doctype
        
//        let element () =
//            match "" with
//            | "<" -> ()
        
        let document () =
            seq {
                yield! prolog()
//                yield! element()
//                
//                while isMisc() do
//                    yield misc() 
            }
        
        yield! document()
    }

"<?xml".EnumerateRunes()
|> normalizeLineEndings
|> parseXml
|> Seq.iter (fun t -> printfn $"{t}")

