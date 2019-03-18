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
import Bootstrap.Navbar as Navbar
import Json.Decode as Json

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

type alias Todo =
    { number: Int
    , task : String
    , isDone : Bool
    }

type alias Model =
    { input : String
    , memos: List Todo
    , navbarState : Navbar.State
    }

init : () -> ( Model, Cmd Msg )
init _ =
    let
        (navbarState, navbarCmd)
            = Navbar.initialState NavbarMsg
    in
        ( { input = "", memos = [], navbarState = navbarState }, navbarCmd )

type Msg
    = Input String
    | Submit
    | Check Int Bool
    | NavbarMsg Navbar.State

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Input input ->
            ( { model | input = input }, Cmd.none )

        Submit ->
            ( { model
                | input = ""
                , memos = (Todo (List.length model.memos + 1) model.input False) :: model.memos
            }, Cmd.none )
        
        Check idx isDone ->
            ( { model
                | memos = (List.map (\todo ->
                    if todo.number == idx then
                        {todo | isDone = isDone}
                    else
                        todo
                    ) model.memos)
            }, Cmd.none )
        
        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Alert.simpleSecondary [] [
            h1 [ class "text-center" ] [ text "Todo" ]
        ]
        , Navbar.config NavbarMsg
            |> Navbar.withAnimation
            |> Navbar.brand [ href "#" ] [ text "Brand" ]
            |> Navbar.items
                [ Navbar.itemLink [ href "#" ] [ text "Item 1" ]
                , Navbar.itemLink [ href "#" ] [ text "Item 2" ]
                ]
            |> Navbar.view model.navbarState
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

subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navbarState NavbarMsg