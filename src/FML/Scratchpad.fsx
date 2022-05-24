open System
open System.Text
open System.Collections.Generic

type Position =
    {
        Line : int64
        LineBegin : int64
        Index : int64
    }

type Character =
    {
        Rune : Rune
        Position : Position
    }

type Transaction = Transaction of int64

type CharacterBuffer() =
    member this.Add () = ()
    
    member this.Remove () = ()
    
    member this.RuneAt () = ()
    
    member this.BeginTransaction() = ()
    
    member this.Rollback() = ()
    
    member this.Commit() = ()

type ReadStatus =
    | NotStarted
    | Started of int64
    | EOF of int64

type CharacterStream(characters: Character seq) =
    let enumerator = characters.GetEnumerator()
    let mutable disposed = false
    let buffer = Dictionary<int64, Character>()
    let mutable transactions: Transaction list = []
    let mutable isEndOfStream = false
    
    let mutable pointer = 0L
    let mutable current = buffer[pointer]
    
    member this.LineNumber
        with get () = current.Position.Line
    
    member this.LineBegin
        with get () = current.Position.LineBegin
    
    member this.Index
        with get () = current.Position.Index
    
    member this.Column
        with get () = this.Index - this.LineBegin + 1L
    
    member this.IsEndOfStream
        with get () = isEndOfStream
        
//    
//    member this.IsBacktrackingStarted
//        with get () = backtrackingStarted
//    
//    member this.StartBacktracking() =
//        backtrackingStarted <- true
//    
//    member this.Rollback() =
//        if backtrackingStarted then
//            match buffer with
//            | [] ->
//                backtrackingStarted <- false
//            | head :: tail ->
//                current <- head
//                buffer <- tail
//                backtrackingStarted <- false
//    
//    member this.FinishBacktracking() =
//        if backtrackingStarted then
//            buffer <- []
//            backtrackingStarted <- false

    member private this.nextChar () =
        if enumerator.MoveNext()
        then
            Some enumerator.Current
        else
            isEndOfStream <- true
            None
//    
//    member private this.getFromBuffer pointer =
//        if not (buffer.ContainsKey pointer) then
//            let mutable found = false
//            
//            while (not isEndOfStream) && (not found) do
//                match this.nextChar() with
//                | Some character ->
//                    let index = character.Position.Index
//                    found <- index = pointer
//                    buffer.Add(index, character)
//                | None ->
//                    ()
//        
//        if not (buffer.ContainsKey pointer) then
//            None
//        else
//            Some buffer.[pointer]


    
    member this.AdvancePointer () =
        let nextPointer = pointer + 1L
        
        if not (buffer.ContainsKey nextPointer) then
            match this.nextChar () with
            | Some character ->
                let index = character.Position.Index
                
                if nextPointer <> index then
                    failwith "Something very wrong happened"
                
                buffer.Add (nextPointer, character)
                pointer <- nextPointer
            | None ->
                ()
    
    
    member this.Peek () =
        if buffer.ContainsKey pointer then
            Some buffer.[pointer].Rune
        else
            None
    
    member this.Match rune =
        match this.Peek () with
        | Some currentRune ->
            currentRune = rune
        | None ->
            false
    
    member this.Skip () =
        match this.nextChar() with
        | Some character ->
            buffer.Remove(pointer) |> ignore
            
            let index = character.Position.Index
            buffer.Add(index, character)
            pointer <- index
        | None ->
            ()

    member this.Skip rune =
        let matched = this.Match rune
        
        if matched then this.Skip()
        
        matched

            
//    
//    member this.Peek2 () =
//        let nextPointer = pointer + 1L
//        
//        if not (buffer.ContainsKey nextPointer) then
//            
//            let nextCharacter = this.nextChar()
//            buffer.Add(nextPointer, nextCharacter)
//
//        buffer.[pointer], buffer.[nextPointer]
    
//    member private this.nextCharacter() =
//        if enumerator.MoveNext()
//        then
//            let character = enumerator.Current
//            pointer <- character.Position.Index
//            buffer.Add (pointer, Some character)
//    
//    member this.Next () =
//        let nextIndex =
//            current
//            |> Option.map (fun c -> c.Position.Index + 1L)
//            |> Option.defaultValue 0L
//        
//        let nextCharInBuffer =
//            buffer
//            |> List.choose id
//            |> List.tryFind (fun c -> c.Position.Index = nextIndex)
//            
//        match backtrackingStarted, nextCharInBuffer with
//        | true, Some character ->
//            current <- Some character
//        | true, None ->
//            let nextCharacter = this.nextCharacter()
//            
//            let newBuffer = buffer @ [ nextCharacter ]
//            
//            current <- nextCharacter
//            buffer <- newBuffer
//        | false, Some character ->
//            let newBuffer =
//                buffer
//                |> List.filter (fun c -> c <> Some character)
//            
//            current <- Some character
//            buffer <- newBuffer
//        | false, None ->
//            current <- this.nextCharacter()

    
    member this.Dispose(disposing) =
        if not disposed then
            if disposing then
                enumerator.Dispose()
            
            disposed <- true
    
    interface IDisposable with
        member this.Dispose() =
            this.Dispose(true)
            GC.SuppressFinalize(this)


type ParserLabel = string
type ParserError = string

type ParseResult<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError

type Parser<'a> =
    {
        ParserFn : (CharacterStream -> ParseResult<'a * CharacterStream>)
        Label : ParserLabel
    }

let prune rune =
    let innerFn (stream: CharacterStream) =
        let c = stream.Peek()
        match c with
        | Some r when r = rune ->
            stream.Skip()
            Success (r, stream)
        | Some r ->
            Failure ("pchar", $"Expected {string rune}")
        | None ->
            Failure ("pchar", "End of stream")
    {ParserFn = innerFn; Label = ""}

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



let newPosition =
    {
        Line = 1
        LineBegin = 0
        Index = 0
    }

let incrIndex pos =
    {pos with Index = pos.Index + 1L}

let incrLine pos =
    let newIndex = pos.Index + 1L
    {Line=pos.Line + 1L; LineBegin=newIndex; Index=newIndex}

let column pos =
    pos.Index - pos.LineBegin + 1L 



let decorateWithPosition (runes: Rune seq) =
    seq {
        let newlineRune = Rune('\n')
        use i = runes.GetEnumerator()
        let mutable nextPosition = newPosition
        
        while i.MoveNext() do
            let currentPos = nextPosition
            
            nextPosition <-
                if i.Current = newlineRune
                then incrLine currentPos
                else incrIndex currentPos

            yield { Rune = i.Current; Position = currentPos }
    }

type Token =
    | XmlDecl



type ParserState =
    {
        Current : Character option
        Buffer : Character option list
        InTransaction : bool 
    }



let initialState =
    {
        Current = None
        Buffer = []
        InTransaction = false
    }

let startTransaction state =
    let buffer = state.Current :: state.Buffer
    {state with Buffer = buffer; InTransaction = true}

let rollback state =
    match state.Buffer with
    | [] ->
        {state with InTransaction = false}
    | head :: tail ->
        {state with Current = head; Buffer = tail; InTransaction = false}

let commit state =
    {state with Buffer = []; InTransaction = false }


let pchar charToMatch =
    let innerFn next input =
        match input.Current with
        | None -> Failure ("pchar", "End of input")
        | Some character when character.Rune = charToMatch ->
            let nextState = next input
            Success (character, nextState)
        | Some character ->
            Failure ("pchar", $"Expected {charToMatch}. Got {character.Rune}")
    
    {ParserFn = innerFn; Label = "pchar"}

        

let parse (runes: Character seq) =
    use i = runes.GetEnumerator()
    
    let next state =
        let nextIndex =
            state.Current
            |> Option.map (fun c -> c.Position.Index + 1)
            |> Option.defaultValue 0
        
        let nextCharInBuffer =
            state.Buffer
            |> List.choose id
            |> List.tryFind (fun c -> c.Position.Index = nextIndex)
            
            
        match state.InTransaction, nextCharInBuffer with
        | true, Some character ->
            {state with Current = Some character}
        | true, None ->
            let nextCharacter =
                if i.MoveNext()
                then Some i.Current
                else None
            
            let newBuffer = state.Buffer @ [ nextCharacter ]
            
            {state with Current = nextCharacter; Buffer = newBuffer }
        | false, Some character ->
            let newBuffer =
                state.Buffer
                |> List.filter (fun c -> c <> Some character)
            
            {state with Current = Some character; Buffer = newBuffer}
        | false, None ->
            if i.MoveNext()
            then {state with Current = Some i.Current}
            else {state with Current = None}

    let logState state =
        printfn $"{state}"
        state
    
    let next' = next >> logState
    let st = startTransaction >> logState
    let rb = rollback >> logState
    let cm = commit >> logState
    
    initialState
    |> next'
    |> st
    |> next'
    |> next'
    |> rb
    |> next'
    |> st
    |> next'
    |> next'
    |> next'
    |> cm
    |> next'
    |> st
    |> rb
    |> next'

let run (parser: Parser<'a>) (characters: Character seq) =
    parser.ParserFn initialState


"abcdefghi".EnumerateRunes()
|> decorateWithPosition
|> parse


for i = 1 to 5 do
    ()

//let parseXml (runes: Rune seq) =
//    seq {
//        use i = runes.GetEnumerator()
//        let k = 5
//        let mutable position = {line=1; column=0}
//        let mutable p = 0
//        
//        let lookahead: Rune option array = Array.create k None
//        
//        let consume () =
//            let next =
//                if i.MoveNext()
//                then
//                    position <-
//                        if i.Current <> Rune('\n')
//                        then incrCol position
//                        else incrLine position
//                    
//                    Some i.Current
//                else None
//            
//            lookahead.[p] <- next
//            p <- (p+1) % k
//        
//        [1..k]
//        |> List.iter (fun _ -> consume())
//        
//        let la i =
//           lookahead.[(p+i-1) % k] 
//        
//        let match' rune =
//            if Some rune = la 1
//            then
//                consume()
//            else
//                failwith $"Expecting {string rune}; found {string (la 1)}"
//        
//        let (|Literal|_|) (c: char) rune =
//            if (Rune c) = rune
//            then Some rune
//            else None
////        
////        let (|InCharList|_|) (list: char list) (rune: Rune) =
////            let runes =
////                list
////                |> List.map Rune
////            
////            if List.contains rune runes
////            then Some rune
////            else None
////        
////        let (|InRange|_|) s e (rune: Rune) =
////            if rune.Value >= s && rune.Value <= e
////            then Some rune
////            else None
////        
////        let isNameStartChar (r: Rune) =
////            let capitalAtoZ = ['A'..'Z']
////            let lowercaseAtoZ = ['a'..'z']
////            
////            match r with
////            | Literal ':' _
////            | InCharList capitalAtoZ _
////            | Literal '_' _
////            | InCharList lowercaseAtoZ _
////            | InRange 0xC0 0xD6 _ -> true
////        
////        let xmlDecl () =
////            ()
////        
////        let misc () =
////            match "" with
////            | "<!--" -> () // comment
////            | "<?" -> () // processing instruction
////            | 0x20 | 0x9 | 0xD | 0xA -> () // whitespace
//        
//        let isWhitespace () =
//            la 1 = Some (Rune 0x20) ||
//            la 1 = Some (Rune 0x9) ||
//            la 1 = Some (Rune 0xD) ||
//            la 1 = Some (Rune 0xA)
//        
////        let (|Whitespace|_|) (rune: Rune option) =
////            match rune with
////            | Some (Rune 0x20)
////            | Some (Rune 0x9)
////            | Some (Rune 0xD)
////            | Some (Rune 0xA) -> rune
////            | _ -> None
//
//        let matchString (str: string) =
//            str.EnumerateRunes()
//            |> Seq.iter match'
//            
//        
//        let matchWhitespace () =
//            if la 1 = Some (Rune 0x20) then match' (Rune 0x20)
//            elif la 1 = Some (Rune 0x9) then match' (Rune 0x9)
//            elif la 1 = Some (Rune 0xD) then match' (Rune 0xD)
//            elif la 1 = Some (Rune 0xA) then match' (Rune 0xA)
//            failwith "Expected whitespace"
//        
//        let isXmlDecl () =
//            la 1 = Some (Rune '<') &&
//            la 2 = Some (Rune '?') &&
//            la 3 = Some (Rune 'x') &&
//            la 4 = Some (Rune 'm') &&
//            la 5 = Some (Rune 'l')
//        
//        let consumeWhitespace () =
//            while isWhitespace() do
//                consume()
//        
//        let matchSingleQuote () =
//            match' (Rune '\'')
//        
//        let matchDoubleQuote () =
//            match' (Rune '"')
//        
//        let isDigit() =
//            ['0'..'9']
//            |> List.map Rune
//            |> List.map Some
//            |> List.contains (la 1)
//        
//        let matchDigit() =
//            match la 1 with
//            | Some rune ->
//                match rune with
//                | Literal '0' r
//                | Literal '1' r
//                | Literal '2' r
//                | Literal '3' r
//                | Literal '4' r
//                | Literal '5' r
//                | Literal '6' r
//                | Literal '7' r
//                | Literal '8' r
//                | Literal '9' r ->
//                    match' r
//                    Ok r
//                | _ ->
//                    Error "Expected digit"
//                 
//                
//        
//        let matchXmlDecl () =
//            
//            
//            let matchEq () =
//                consumeWhitespace ()
//                match' (Rune '=')
//                consumeWhitespace ()
//            
//            let matchVersionNumber () =
//                matchString "1."
//                let builder = StringBuilder("1.")
//                
//                if isDigit() then
//                    builder.Append (string (la 1)) |> ignore
//                    matchDigit()
//                    
//                    while isDigit() do
//                        builder.Append (string (la 1)) |> ignore
//                        matchDigit()
//
//                
//            let matchVersion () =
//                matchWhitespace()
//                matchString "version"
//                matchEq()
//                
//                if la 1 = Some (Rune ('\'')) then
//                    matchSingleQuote()
//                    
//                    matchSingleQuote()
//                else
//                    matchDoubleQuote()
//                    
//                    matchDoubleQuote()
//            
//            matchString "<?xml"
//            matchVersion()
//
//            Ok XmlDecl
//        
//        let element () =
//            seq {
//                Ok XmlDecl
//            }
//        
//        let isMisc() = false
//        
//        let misc() = Ok XmlDecl
//        
//        let prolog () =
//            seq {
//                if isXmlDecl() then yield matchXmlDecl()
//            }
//                
//                
//            
////            match "" with
////            | "<?xml" -> () // XMLDecl
////            | "<!DOCTYPE" -> () // doctype
//        
////        let element () =
////            match "" with
////            | "<" -> ()
//        
//        let document () =
//            seq {
//                yield! prolog()
////                yield! element()
////                
////                while isMisc() do
////                    yield misc() 
//            }
//        
//        yield! document()
//    }

"<?xml\r\nblahblah\rblah".EnumerateRunes()
|> normalizeLineEndings
|> decorateWithPosition
//|> Seq.map (fun (c,p) -> c, p, column p)
//|> parseXml
|> Seq.iter (fun t -> printfn $"{t}")