module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Array exposing (Array)

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }

type alias Model =
  Array Int

init: Model
init =
  Array.fromList [0, 0, 0]

type Msg
  = Increment Int
  | Decrement Int
  | Reset Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment index ->
      case Array.get index model of
        Just value ->
          Array.set index (value + 1) model
        Nothing ->
          model
    Decrement index ->
      case Array.get index model of
        Just value ->
          Array.set index (value - 1) model
        Nothing ->
          model
    Reset index ->
      Array.set index 0 model

view : Model -> Html Msg
view model =
  div [] <|
    Array.toList <| Array.indexedMap (\index value -> buttons index value) model

buttons : Int -> Int -> Html Msg
buttons index value =
  div [ style "display" "inline-block" ]
    [ button [ onClick (Decrement index) ] [ text "-" ]
    , div [ onClick (Reset index) ] [ text (String.fromInt value) ]
    , button [onClick (Increment index) ] [ text "+" ]
    ]
