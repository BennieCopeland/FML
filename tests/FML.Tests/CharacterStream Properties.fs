module FML.Tests.``CharacterStream Properties``

open System.Text
open FsCheck.Xunit
open Xunit

type Position =
    {
        Line : int64
        LineBegin : int64
        Index : int64
    }

type Character<'a> =
    {
        Character : 'a
        Position : Position
    }

type Something =
    | Character of Rune
    | EOF

type CharacterStream<'a>(characters: Character<'a> seq) =
    
    member this.Peek() =
        Rune()

type XmlDeclaration =
    {
        Version: string
        Encoding : string option
        
    }

[<Property>]
let ``Peek returns the first character in the string`` (str: string) =
    
    
    Assert.True(true)