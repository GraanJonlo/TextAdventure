namespace Domain

module Words =
    type Determiner =
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

    type Noun = Noun of string

    type Verb = Verb of string

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
        head = Verb "pick"
        complement = Object theRedKey
    }
