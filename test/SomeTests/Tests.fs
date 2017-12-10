module Tests

open Domain.TextAdventureDomain
open Domain.ConsoleUi
open Xunit

[<Fact>]
let ``adjectives rendered in correct order`` () =
    let np = {
        head = Noun "key"
        adjectives = [Colour "red"; Size "big"]
        determiner = Some A
    }

    Assert.Equal("a big, red key", (npToString np))
