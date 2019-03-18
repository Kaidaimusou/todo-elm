import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Alert as Alert
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Button as Button
import Bootstrap.Table as Table
import Json.Decode as Json

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }

type alias Todo =
    { number: Int
    , task : String
    , isDone : Bool
    }

type alias Model =
    { input : String
    , memos: List Todo
    }

init : Model
init =
    { input = ""
    , memos = []
    }

type Msg
    = Input String
    | Submit
    | Check Int Bool

update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model | input = input }

        Submit ->
            { model
                | input = ""
                , memos = (Todo (List.length model.memos + 1) model.input False) :: model.memos
            }
        
        Check idx isDone ->
            { model
                | memos = (List.map (\todo ->
                    if todo.number == idx then
                        {todo | isDone = isDone}
                    else
                        todo
                    ) model.memos)
            }

view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Alert.simpleSecondary [] [
            h1 [ class "text-center" ] [ text "Todo" ]
        ]
        , Form.form [ onSubmit Submit ]
            [ Form.group []
                [ Form.label [ for "task" ] [ text "task" ]
                , Input.text [ Input.value model.input, Input.onInput Input ] 
                ]
            , Form.group [] [
                Button.button
                    [ Button.secondary, Button.attrs [disabled (String.length model.input < 1)] ]
                    [ text "Submit" ]
                ]
            ]
        , Table.table
            { options = [ Table.striped, Table.inversed]
            , thead = Table.simpleThead
                [ Table.th [] [ text "Number" ]
                , Table.th [] [ text "Task" ]
                , Table.th [] [ text "isDone" ]
                ]
            , tbody = 
                Table.tbody [] (List.map viewMemo model.memos)
            }
        ]

viewMemo : Todo -> Table.Row Msg
viewMemo todo =
    Table.tr []
        [ Table.td [] [ text <| String.fromInt todo.number ]
        , Table.td [] [ text todo.task ]
        , Table.td []
            [ input
                [ id (String.fromInt todo.number)
                , type_ "checkbox"
                , checked todo.isDone
                , onClick (Check todo.number (not todo.isDone))
                ] [ text "Done" ]
            ]
        ]