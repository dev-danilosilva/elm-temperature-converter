module Main exposing (..)

import Html exposing (Attribute, Html, article, div, figure, img, input, option, p, section, select, text)
import Html.Events exposing (onInput)
import Html.Attributes exposing (class, placeholder, src, type_, value)
import Temperature exposing (TempUnit(..), Temperature, unitFromString, convertFrom, unitToStringShort)
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

        Just unit -> Debug.log "Model: " { model
                                            | from = Temperature unit model.from.scalar
                                            , to   = model.from.scalar
                                                     |> convertFrom unit model.to.unit
                                                     |> Temperature model.to.unit
                                         }

    ChangeToUnit u -> case unitFromString u of

        Nothing -> model

        Just toUnit -> Debug.log "Model: " { model | to =
                                                model.from.scalar
                                                |> convertFrom model.from.unit toUnit
                                                |> Temperature toUnit
                                           }


view : Model -> Html Msg
view model =
    div [class "container"]
        [ div [ class "tile is-ancestor mt-5" ]
            [ div [ class "tile is-vertical is-8" ]
                [ div [ class "tile" ]
                    [ div [ class "tile is-parent is-vertical" ]
                        [ article [ class "tile is-child notification is-primary" ]
                            [ p [ class "title" ]
                                [ text "FROM" ]
                            , div [class "field is-grouped"]
                                [ div [class "columns"]
                                    [ div [ class "column is-half"]
                                        [ div [ class "control" ]
                                            [ input [class "input is-large", model.from.scalar |> String.fromFloat |> value, type_ "text", onInput ChangeFromScalar] [] ]
                                        ]
                                    , div [ class "column is-half" ]
                                        [ div [ class "control" ]
                                            [ selectTemperatureUnit ChangeFromUnit ]]
                                    ]
                                ]
                            ]
                        , article [ class "tile is-child notification is-warning" ]
                            [ p [ class "title" ]
                                [ text "TO" ]
                            , div [class "columns"]
                                [ div [ class "column is-8 is-offset-2"]
                                    [ selectTemperatureUnit ChangeToUnit
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "tile is-parent" ]
                        [ article [ class "tile is-flex-direction-column is-flex is-justify-content-center is-align-content-center is-child notification is-info has-text-centered" ]
                            [ p [ class "title" ]
                                [ model.from.scalar |> String.fromFloat |> text ]
                            , p [ class "subtitle" ]
                                [ model.from.unit |> Temperature.unitToStringLong |> text ]
                            ]
                        ]
                    ]
                ]
            , div [ class "tile is-parent" ]
                [ article [ class "tile is-flex-direction-column is-flex is-justify-content-center is-align-content-center has-text-centered is-child notification is-success" ]
                    [ p [ class "title" ]
                        [ model.to.scalar |> String.fromFloat |> text ]
                    , p [ class "subtitle" ]
                        [ model.to.unit |> Temperature.unitToStringLong |> text ]
                    ]
                ]
            ]
        ]


selectTemperatureUnit : (String -> msg) -> Html msg
selectTemperatureUnit message =
    select [ onInput message, class "select is-large"]
            [ option [ value "C"] [text "Celsius"]
            , option [ value "F"] [text "Fahrenheit"]
            , option [ value "K"] [text "Kelvin"]
            ]

initialScalar = 0

initialFromUnit = C

initialToUnit = C

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
