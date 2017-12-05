namespace Domain

module Words =
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
