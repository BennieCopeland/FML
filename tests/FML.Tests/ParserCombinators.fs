module FML.Tests.ParserCombinators

open System.Collections.Generic
open System.Text

module TextInput =
    open System

//    type Position = {
//        line : int
//        column : int
//    }
//
//    /// define an initial position
//    let initialPos = {line=0; column=0}
//
//    /// increment the column number
//    let incrCol (pos:Position) =
//        {pos with column=pos.column + 1}
//
//    /// increment the line number and set the column to 0
//    let incrLine pos =
//        {line=pos.line + 1; column=0}
//
//    /// Define the current input state
//    type InputState = {
//        lines : string[]
//        position : Position
//    }
//
//    // return the current line
//    let currentLine inputState =
//        let linePos = inputState.position.line
//        if linePos < inputState.lines.Length then
//            inputState.lines.[linePos].EnumerateRunes()
//            |> List.ofSeq
//        else
//            "end of file"
//
//    /// Create a new InputState from a string
//    let fromStr str =
//        if String.IsNullOrEmpty(str) then
//            {lines=[||]; position=initialPos}
//        else
//            let normalizedStr = str.Replace("\r\n", "\n").Replace("\r", "\n")
//            let separators = [| "\n" |]
//            let lines = normalizedStr.Split(separators, StringSplitOptions.None)
//            {lines=lines; position=initialPos}
//
//
//    /// Get the next character from the input, if any
//    /// else return None. Also return the updated InputState
//    /// Signature: InputState -> InputState * char option
//    let nextChar input =
//        let linePos = input.position.line
//        let colPos = input.position.column
//        // three cases
//        // 1) if line >= maxLine ->
//        //       return EOF
//        // 2) if col less than line length ->
//        //       return char at colPos, increment colPos
//        // 3) if col at line length ->
//        //       return NewLine, increment linePos
//
//        if linePos >= input.lines.Length then
//            input, None
//        else
//            let currentLine = currentLine input
//            if colPos < currentLine.EnumerateRunes().Count() then
//                let char = currentLine.[colPos]
//                let newPos = incrCol input.position
//                let newState = {input with position=newPos}
//                newState, Some char
//            else
//                // end of line, so return LF and move to next line
//                let char = '\n'
//                let newPos = incrLine input.position
//                let newState = {input with position=newPos}
//                newState, Some char

    type Position =
        {
            line : int
            column : int
        }
    
    /// define an initial position
    let initialPos = {line=1; column=0}
    
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

    let fromSeq (runes : Rune seq) =
        { Iterator = runes.GetEnumerator(); Position = initialPos }
    
    let nextRune input =
        let hasRune = input.Iterator.MoveNext()
        
        if hasRune then
            let newPos =
                if input.Iterator.Current <> Rune('\n') then
                    incrCol input.Position
                else
                    incrLine input.Position
            let newState = {input with Position=newPos}
            printfn $"Current: {input.Iterator.Current}"
            newState, Some input.Iterator.Current
        else
            input, None

// ===========================================
// Parser code
// ===========================================

open System

// Aliases for input, etc
type Input = TextInput.InputState   // type alias
type ParserLabel = string
type ParserError = string

/// Stores information about the parser position for error messages
type ParserPosition = {
    line : int
    column : int
    }

// Result type
type ParseResult<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * ParserPosition

/// A Parser structure has a parsing function & label
type Parser<'a> = {
    parseFn : Input -> ParseResult<'a * Input>
    label:  ParserLabel
    }


/// Run the parser on a InputState
let runOnInput parser input =
    // call inner function with input
    parser.parseFn input

/// Run the parser on a string
//let run parser inputStr =
//    // call inner function with input
//    runOnInput parser (TextInput.fromStr inputStr)

// =============================================
// Error messages
// =============================================

let parserPositionFromInputState (inputState:Input) = {
    line = inputState.Position.line
    column = inputState.Position.column
    }

let printResult result =
    match result with
    | Success (value,_) ->
        printfn $"%A{value}"
    | Failure (label,error,parserPos) ->
        let colPos = parserPos.column
        let linePos = parserPos.line
        let failureCaret = sprintf "%*s^%s" colPos "" error
        // examples of formatting
        //   sprintf "%*s^%s" 0 "" "test"
        //   sprintf "%*s^%s" 10 "" "test"
        printfn $"Line:%i{linePos} Col:%i{colPos} Error parsing %s{label}\n%s{failureCaret}"


// =============================================
// Label related
// =============================================

/// get the label from a parser
let getLabel parser =
    // get label
    parser.label

/// update the label in the parser
let setLabel parser newLabel =
    // change the inner function to use the new label
    let newInnerFn input =
        let result = parser.parseFn input
        match result with
        | Success s ->
            // if Success, do nothing
            Success s
        | Failure (_,err,pos) ->
            // if Failure, return new label
            Failure (newLabel,err,pos)
    // return the Parser
    {parseFn=newInnerFn; label=newLabel}

/// infix version of setLabel
let ( <?> ) = setLabel


// =============================================
// Standard combinators
// =============================================

/// Match an input token if the predicate is satisfied
let satisfy predicate label =
    let innerFn input =
        let remainingInput,runeOpt = TextInput.nextRune input
        printfn $"{runeOpt} {predicate}"
        match runeOpt with
        | None ->
            let err = "No more input"
            let pos = parserPositionFromInputState input
            Failure (label,err,pos)
        | Some first ->
            if predicate first then
                Success (first,remainingInput)
            else
                let err = $"Unexpected '{first.ToString()}'"
                let pos = parserPositionFromInputState input
                Failure (label,err,pos)
    // return the parser
    {parseFn=innerFn;label=label}

/// "bindP" takes a parser-producing function f, and a parser p
/// and passes the output of p into f, to create a new parser
let bindP f p =
    let label = "unknown"
    let innerFn input =
        let result1 = runOnInput p input
        match result1 with
        | Failure (label,err,pos) ->
            // return error from parser1
            Failure (label,err,pos)
        | Success (value1,remainingInput) ->
            // apply f to get a new parser
            let p2 = f value1
            // run parser with remaining input
            runOnInput p2 remainingInput
    {parseFn=innerFn; label=label}

/// Infix version of bindP
let ( >>= ) p f = bindP f p

/// Lift a value to a Parser
let returnP x =
    let label = $"%A{x}"
    let innerFn input =
        // ignore the input and return x
        Success (x,input)
    // return the inner function
    {parseFn=innerFn; label=label}

/// apply a function to the value inside a parser
let mapP f =
    bindP (f >> returnP)

/// infix version of mapP
let ( <!> ) = mapP

/// "piping" version of mapP
let ( |>> ) x f = mapP f x

/// apply a wrapped function to a wrapped value
let applyP fP xP =
    fP >>= (fun f ->
    xP >>= (fun x ->
        returnP (f x) ))

/// infix version of apply
let ( <*> ) = applyP

/// lift a two parameter function to Parser World
let lift2 f xP yP =
    returnP f <*> xP <*> yP

/// Combine two parsers as "A andThen B"
let andThen p1 p2 =
    let label = $"%s{getLabel p1} andThen %s{getLabel p2}"
    p1 >>= (fun p1Result ->
    p2 >>= (fun p2Result ->
        returnP (p1Result,p2Result) ))
    <?> label

/// Infix version of andThen
let ( .>>. ) = andThen

/// Combine two parsers as "A orElse B"
let orElse p1 p2 =
    let label = $"%s{getLabel p1} orElse %s{getLabel p2}"
    let innerFn input =
        // run parser1 with the input
        let result1 = runOnInput p1 input

        // test the result for Failure/Success
        match result1 with
        | Success _ ->
            // if success, return the original result
            result1

        | Failure _ ->
            // if failed, run parser2 with the input
            let result2 = runOnInput p2 input

            // return parser2's result
            result2

    // return the inner function
    {parseFn=innerFn; label=label}

/// Infix version of orElse
let ( <|> ) = orElse

//let andNot p1 p2 =
//    let label = $"%s{getLabel p1} andNot %s{getLabel p2}"
//    let innerFn input =
//        // run parser1 with the input
//        let result1 = runOnInput p1 input
//        let result2 = runOnInput p2 input
//
//        // test the result for Failure/Success
//        match result1, result2 with
//        | Success _, Failure _ ->
//            // if success, return the original result
//            result1
//        | Success _, Success _ ->
//            let err = "Parsed input where it was not expected"
//            let position = parserPositionFromInputState input
//            Failure (label, err, position)
//
//        | Failure (s, s1, parserPosition), _ ->
//            Failure (s, s1, parserPosition)
//    
//    // return the inner function
//    {parseFn=innerFn; label=label}

/// Choose any of a list of parsers
let choice listOfParsers =
    List.reduce ( <|> ) listOfParsers

let rec sequence parserList =
    // define the "cons" function, which is a two parameter function
    let cons head tail = head::tail

    // lift it to Parser World
    let consP = lift2 cons

    // process the list of parsers recursively
    match parserList with
    | [] ->
        returnP []
    | head::tail ->
        consP head (sequence tail)

/// (helper) match zero or more occurrences of the specified parser
let rec parseZeroOrMore parser input =
    // run parser with the input
    let firstResult = runOnInput parser input
    // test the result for Failure/Success
    match firstResult with
    | Failure _ ->
        // if parse fails, return empty list
        ([],input)
    | Success (firstValue,inputAfterFirstParse) ->
        // if parse succeeds, call recursively
        // to get the subsequent values
        let subsequentValues,remainingInput =
            parseZeroOrMore parser inputAfterFirstParse
        let values = firstValue::subsequentValues
        (values,remainingInput)

/// matches zero or more occurrences of the specified parser
let many parser =
    let label = $"many %s{getLabel parser}"
    let innerFn input =
        // parse the input -- wrap in Success as it always succeeds
        Success (parseZeroOrMore parser input)
    {parseFn=innerFn; label=label}

/// matches one or more occurrences of the specified parser
let many1 p =
    let label = $"many1 %s{getLabel p}"

    p      >>= (fun head ->
    many p >>= (fun tail ->
    returnP (head::tail) ))
    <?> label

/// Parses an optional occurrence of p and returns an option value.
let opt p =
    let label = $"opt %s{getLabel p}"
    let some = p |>> Some
    let none = returnP None
    (some <|> none) <?> label

/// Keep only the result of the left side parser
let (.>>) p1 p2 =
    // create a pair
    p1 .>>. p2
    // then only keep the first value
    |> mapP fst

/// Keep only the result of the right side parser
let (>>.) p1 p2 =
    // create a pair
    p1 .>>. p2
    // then only keep the second value
    |> mapP snd

/// Keep only the result of the middle parser
let between p1 p2 p3 =
    p1 >>. p2 .>> p3

/// Parses one or more occurrences of p separated by sep
let sepBy1 p sep =
    let sepThenP = sep >>. p
    p .>>. many sepThenP
    |>> fun (p,pList) -> p::pList

/// Parses zero or more occurrences of p separated by sep
let sepBy p sep =
    sepBy1 p sep <|> returnP []

// =============================================
// Standard parsers
// =============================================


// ------------------------------
// char and string parsing
// ------------------------------

/// parse a char
let pchar (runeToMatch: Rune) =
    // label is just the character
    let label = $"{runeToMatch.ToString()}"

    let predicate ch = (ch = runeToMatch)
    satisfy predicate label

/// Choose any of a list of characters
let anyOf listOfChars =
    let label = $"anyOf %A{listOfChars}"
    listOfChars
    |> List.map pchar // convert into parsers
    |> choice
    <?> label

/// Convert a list of chars to a string
let charListToStr runeList =
    runeList
    |> List.map (fun r -> r.ToString())
    |> List.reduce (fun a b -> a + b)

/// Parses a sequence of zero or more chars with the char parser cp.
/// It returns the parsed chars as a string.
let manyChars cp =
    many cp
    |>> charListToStr

/// Parses a sequence of one or more chars with the char parser cp.
/// It returns the parsed chars as a string.
let manyChars1 cp =
    many1 cp
    |>> charListToStr

/// parse a specific string
let pstring (str: string) =
    // label is just the string
    let label = str

    str.EnumerateRunes()
    // convert to list of char
    |> List.ofSeq
    |> List.map (fun r -> printfn $"Pstring Rune: {r}"; r)
    // map each char to a pchar
    |> List.map pchar
    // convert to Parser<char list>
    |> sequence
    // convert Parser<char list> to Parser<string>
    |> mapP charListToStr
    <?> label

// ------------------------------
// whitespace parsing
// ------------------------------

/// parse a whitespace char
let whitespaceChar =
    let predicate = Rune.IsWhiteSpace
    let label = "whitespace"
    satisfy predicate label

/// parse zero or more whitespace char
let spaces = many whitespaceChar

/// parse one or more whitespace char
let spaces1 = many1 whitespaceChar



// ------------------------------
// number parsing
// ------------------------------

/// parse a digit
let digitChar =
    let predicate = Rune.IsDigit
    let label = "digit"
    satisfy predicate label


// parse an integer
let pint =
    let label = "integer"

    // helper
    let resultToInt (sign,digits) =
        let i = digits |> int  // ignore int overflow for now
        match sign with
        | Some _ -> -i  // negate the int
        | None -> i

    // define parser for one or more digits
    let digits = manyChars1 digitChar

    // an "int" is optional sign + one or more digits
    opt (pchar (Rune '-')) .>>. digits
    |> mapP resultToInt
    <?> label

// parse a float
let pfloat =
    let label = "float"

    // helper
    let resultToFloat (((sign,digits1),point),digits2) =
        let fl = $"%s{digits1}.%s{digits2}" |> float
        match sign with
        | Some ch -> -fl  // negate the float
        | None -> fl

    // define parser for one or more digits
    let digits = manyChars1 digitChar

    // a float is sign, digits, point, digits (ignore exponents for now)
    opt (pchar (Rune '-')) .>>. digits .>>. pchar (Rune '.') .>>. digits
    |> mapP resultToFloat
    <?> label




module Xml =
    
    let lowercaseAtoZ =
        ['a'..'z']
        |> List.map Rune
        |> anyOf
    
    let uppercaseAtoZ =
        ['A'..'Z']
        |> List.map Rune
        |> anyOf
    
    let zeroToNine =
        ['0'..'9']
        |> List.map Rune
        |> anyOf
    
    let whiteSpace =
        [ 0x20; 0x9; 0xD; 0xA]
        |> List.map Rune
        |> anyOf
        |> many1
        <?> "White Space"
    
//    let char =
//        [
//            pchar (char 0x9)
//            pchar (char 0xa)
//            pchar (char 0xd)
//            [0x20..0xd7ff] |> List.map char |> anyOf
//            [0xe000..0xfffd] |> List.map char |> anyOf
//            [0x10000..0x10FFFF] |> List.map char |> anyOf
//        ]
//        |> choice
        
//    let char2 =
//        let inner (c: char) =
//            System.Char.
//            c > 0x20
//        
//        satisfy inner ""
            
    
    let eq =
        (opt whiteSpace) .>>. pchar( Rune '=') .>>. (opt whiteSpace)
    
    let versionNum =
        pstring "1." .>>. (zeroToNine |> many1)

    let singleQuote = pchar (Rune ''')
    let doubleQuote = pchar (Rune '"')
        
    let versionInfo =
        let a = singleQuote .>>. versionNum .>>. singleQuote
        let b = doubleQuote .>>. versionNum .>>. doubleQuote
        
        whiteSpace .>>. pstring "version"// .>>. eq .>>. (a <|> b)
    
    let lexer (l: char seq) =
        seq {
            use i = l.GetEnumerator()
            let mutable hasNext = i.MoveNext()
            
            let consume() =
                hasNext <- i.MoveNext()
            
            let ws() =
                while i.Current = ' ' do
                    consume()
            
            let isLetter() =
                let letters = ['a'..'z']
                List.contains i.Current letters

            let name() =
                let charListToStr charList =
                    System.String(List.toArray charList)
                
                let rec loop s =
                    if isLetter()
                    then
                        let newS = i.Current :: s
                        consume()
                        loop newS
                    else
                        s
                
                loop [] |> List.rev |> charListToStr
            
            while hasNext do
                match i.Current with
                | ' ' -> ws()
                | '<' ->
                    consume()
                    yield "LANGLE"
                | s when isLetter() ->
                    yield $"Name: {name()}"
                | s ->
                    consume()
                    yield $"UNKNOWN {s}"
        }

    "  <ab123"
    |> lexer
    |> Seq.iter (fun s -> printfn $"{s}")
    
    let encName =
        let firstLetter = [uppercaseAtoZ; lowercaseAtoZ] |> choice
        
        let remainingLetters =
            [lowercaseAtoZ; uppercaseAtoZ; zeroToNine; pchar (Rune '.'); pchar (Rune '_')]
            |> choice
        
        firstLetter .>>. many (remainingLetters <|> pchar (Rune '-'))
    
    let encodingDecl =
        let a = singleQuote .>>. encName .>>. singleQuote
        let b = doubleQuote .>>. encName .>>. doubleQuote
        whiteSpace .>>. pstring "encoding" .>>. eq .>>. (a <|> b)
    
    let sddecl =
        let yes = pstring "yes"
        let no = pstring "no"
        let a = singleQuote .>>. (yes <|> no) .>>. singleQuote
        let b = doubleQuote .>>. (yes <|> no) .>>. doubleQuote
        whiteSpace .>>. pstring "standalone" .>>. eq .>>. (a <|> b)
    
    let xmlDecl =
        pstring "<?xml" .>>. versionInfo //.>>. opt encodingDecl .>>. opt sddecl .>>. opt whiteSpace .>>. pstring "?>"
//
//    let dash = pchar '-'
//    let charNoDash = andNot char dash
//    let b = (dash .>>. charNoDash) |> mapP (fun (a, b) -> [a; b] |> charListToStr)
//    
//    let comment =
//        
//        pstring "<!--" .>>. ((charNoDash |> mapP string) <|> b) |> many .>>. pstring "-->"
//    
//    let misc =
//        choice [comment]
//    
//    let prolog =
//        opt xmlDecl


//"<?xml version='1.05135131' encoding='blah' standalone = 'yes' ?>".EnumerateRunes()
" version".EnumerateRunes()// |> Seq.map (fun r -> r.ToString()) |> Seq.iter (fun r -> printfn $"{r}")
|> TextInput.fromSeq
|> runOnInput Xml.versionInfo


//run Xml.xmlDecl "<?xml version='1.05135131' encoding='blah' standalone = 'yes' ?>"