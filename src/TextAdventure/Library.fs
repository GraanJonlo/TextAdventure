namespace Domain

module TextAdventureDomain =
    type Determiner =
    | A
    | The

    type Preposition =
    | Up

    type Adjective =
    | Opinion of string // unusual, lovely, beautiful
    | Size of string // big, small, tall
    | PhysicalQuality of string // thin, rough, untidy
    | Shape of string // round, square, rectangular
    | Age of string // young, old, youthful
    | Colour of string // blue, red, pink
    | Origin of string // Dutch, Japanese, Turkish
    | Material of string // metal, wood, plastic
    | Type of string // general-purpose, four-sided, U-shaped
    | Purpose of string // cleaning, hammering, cooking

    let adjSortOrder =
        function
        | Opinion _ -> 1
        | Size _ -> 2
        | PhysicalQuality _ -> 3
        | Shape _ -> 4
        | Age _ -> 5
        | Colour _ -> 6
        | Origin _ -> 7
        | Material _ -> 8
        | Type _ -> 9
        | Purpose _ -> 10
    
    // Allows List.sortBy adjSortOrder adjectives
    
    type Noun = Noun of string

    type Verb = Verb of string

    type Word =
    | Det of Determiner
    | P of Preposition
    | Adj of Adjective
    | N of Noun
    | V of Verb

    type NounPhrase = {
        head:Noun
        determiner:Determiner option
        adjectives:Adjective list
    }

    type PrepositionalPhrase = {
        head:Preposition
        nounPhrase:NounPhrase
    }

    type VerbComplement =
    | Adverbial of PrepositionalPhrase
    | Object of NounPhrase

    type VerbPhrase = {
        head:Verb
        complement:VerbComplement
    }

    type Item = {
        noun:Noun
        adjectives:Adjective list
        shortDescription:string
        longDescription:string
    }

    let npOfItem x = {
        head = x.noun
        determiner = Some A
        adjectives = x.adjectives
    }

    type ItemMatchResult =
    | Exact of Item
    | Ambiguous of Item list
    | NoMatch

    type MatchItem = Item list -> NounPhrase -> ItemMatchResult

module ConsoleUi =
    open TextAdventureDomain

    let startsWithVowel x =
        x <> "" && List.contains x.[0] ['a';'e';'i';'o';'u']

    let detToString followedByAVowel =
        function
        | A -> if followedByAVowel then "an" else "a"
        | The -> "the"

    let pToString =
        function
        | Up -> "up"

    let adjToString =
        function
        | Opinion x -> x
        | Size x -> x
        | PhysicalQuality x -> x
        | Shape x -> x
        | Age x -> x
        | Colour x -> x
        | Origin x -> x
        | Material x -> x
        | Type x -> x
        | Purpose x -> x

    let adjsToString =
        List.sortBy adjSortOrder
        >> Seq.map adjToString
        >> String.concat ", "
    
    let npToString (x:NounPhrase) =
        let adjStr = adjsToString x.adjectives
        
        let detStr =
            match x.determiner with
            | Some y -> detToString (startsWithVowel adjStr) y
            | None -> ""

        let nounStr =
            match x.head with
            | Noun n -> n

        [detStr; adjStr; nounStr]
        |> Seq.filter (fun x -> x<>"")
        |> String.concat " "

    let theRedKey = {
        head = Noun "key"
        determiner = Some The
        adjectives = [Colour "red"]
    }

    let pickUpTheRedKey = {
        head = Verb "pick"
        complement = Adverbial {
            head = Up
            nounPhrase = theRedKey
        }
    }

    let getTheRedKey = {
        head = Verb "get"
        complement = Object theRedKey
    }
