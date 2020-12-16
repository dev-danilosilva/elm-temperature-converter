module Main exposing (..)

import Html exposing (Attribute, Html, div, input, option, select, text)
import Html.Events exposing (onInput)
import Html.Attributes exposing (class, placeholder, value)
import Temperature exposing (TempUnit(..), Temperature, unitFromString, convertFrom)
import Browser

type alias Model =
    { from : Temperature
    , to   : Temperature
    }

type Msg
    = ChangeFromScalar String
    | ChangeFromUnit String
    | ChangeToUnit String


update : Msg -> Model -> Model
update msg model = case msg of
    ChangeFromScalar t -> case String.toFloat t of
        Nothing -> { model
                        | from = Temperature model.from.unit initialScalar
                        , to = initialScalar
                            |> convertFrom model.from.unit model.to.unit
                            |> Temperature model.to.unit
                   }

        Just scalar -> { model
                            | from = Temperature model.from.unit scalar
                            , to = scalar
                                |> convertFrom model.from.unit model.to.unit
                                |> Temperature model.to.unit
                       }

    ChangeFromUnit u -> case unitFromString u of

        Nothing -> model

        Just unit -> Debug.log "Model: " { model | from = Temperature unit model.from.scalar }

    ChangeToUnit u -> case unitFromString u of

        Nothing -> model

        Just toUnit -> Debug.log "Model: " { model | to =
                                                model.from.scalar
                                                |> convertFrom model.from.unit toUnit
                                                |> Temperature toUnit
                                           }


view : Model -> Html Msg
view model =
    div []
        [ input [placeholder "Scalar", onInput ChangeFromScalar] []
        , selectTemperatureUnit ChangeFromUnit ""
        , selectTemperatureUnit ChangeToUnit ""
        ]

selectTemperatureUnit : (String -> msg) -> String -> Html msg
selectTemperatureUnit message classes =
    select [ onInput message ]
            [ option [] []
            , option [ class classes, value "C"] [text "Celsius"]
            , option [ class classes, value "F"] [text "Fahrenheit"]
            , option [ class classes, value "K"] [text "Kelvin"]
            ]

initialScalar = 0

initialFromUnit = C

initialToUnit = F

initialFromTemperature = Temperature initialFromUnit initialScalar

initialToTemperature =
    initialScalar
    |> convertFrom initialFromUnit initialToUnit
    |> Temperature initialToUnit

main =
    Browser.sandbox
        { init = Model initialFromTemperature initialToTemperature
        , update = update
        , view = view
        }

