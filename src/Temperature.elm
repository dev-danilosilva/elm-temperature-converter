module Temperature exposing
    ( Temperature
    , cToK
    , cToF
    , kToC
    , kToF
    , fToC
    , fToK
    , convertFrom
    , getScalar
    , getUnit
    , TempUnit(..)
    , unitFromString
    )


type TempUnit
    = C
    | K
    | F

type alias Temperature =
    { unit : TempUnit
    , scalar : Float
    }

type alias ConversionMethod =
    Float -> Float

convertTo_: TempUnit -> ConversionMethod -> Temperature -> Temperature
convertTo_ unit f t0 =
    t0.scalar
    |> f
    |> Temperature unit

getScalar : Temperature -> Float
getScalar t = t.scalar

getUnit : Temperature -> TempUnit
getUnit t = t.unit

cToK : Temperature -> Temperature
cToK = convertTo_ K (\c -> c + 273.15)

cToF : Temperature -> Temperature
cToF = convertTo_ F (\c -> (c * (9 / 5)) + 32)

kToC : Temperature -> Temperature
kToC = convertTo_ C (\k -> k - 273.15)

kToF : Temperature -> Temperature
kToF = convertTo_ F (\k -> (k - 273.15) * (9/5) + 32)

fToC : Temperature -> Temperature
fToC = convertTo_ C (\f -> (f - 32) * (5/9))

fToK : Temperature -> Temperature
fToK = convertTo_ K (\f -> (f - 32) * (5/9) + 273.15)

unitFromString : String -> Maybe TempUnit
unitFromString u = case String.toUpper u of
    "K" -> Just K
    "F" -> Just F
    "C" -> Just C
    "Kelvin" -> Just K
    "Fahrenheit" -> Just F
    "Celsius" -> Just C
    _   -> Nothing

convertFrom : TempUnit -> TempUnit -> Float -> Float
convertFrom u0 u1 t0 = case u0 of
    K -> case u1 of
        K -> t0
        F -> (t0 - 273.15) * (9/5) + 32
        C -> t0 - 273.15

    F -> case u1 of
        K -> (t0 - 32) * (5/9) + 273.15
        F -> t0
        C -> (t0 - 32) * (5/9)

    C -> case u1 of
        K -> t0 + 273.15
        F -> (t0 * (9 / 5)) + 32
        C -> t0


