import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode as Json
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>))

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Alert as Alert
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Button as Button
import Bootstrap.Table as Table
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing as Spacing

main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }

type alias Flags =
    {}

type alias Todo =
    { number: Int
    , task : String
    , isDone : Bool
    }

type alias Model =
    { page: Page
    , input : String
    , memos: List Todo
    , navbarState : Navbar.State
    , navKey : Navigation.Key
    , url : Url.Url
    }

type Page
    = UnDone
    | Done
    | NotFound

init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navbarState, navCmd )
            = Navbar.initialState NavbarMsg
        
        ( model, urlCmd ) =
            urlUpdate url { page = UnDone, input = "", memos = [], navbarState = navbarState, navKey = key, url = url }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )

type Msg
    = Input String
    | Submit
    | Check Int Bool
    | NavbarMsg Navbar.State
    | UrlChange Url
    | ClickedLink UrlRequest

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
        
        UrlChange url ->
            urlUpdate url model
        
        ClickedLink req ->
            case req of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.navKey <| Url.toString url )
                
                Browser.External href ->
                    ( model, Navigation.load href )

urlUpdate : Url.Url -> Model -> ( Model, Cmd Msg)
urlUpdate url model =
    case decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )
        Just page ->
            ( { model | page = page }, Cmd.none )

decode : Url.Url -> Maybe Page
decode url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
    |> UrlParser.parse parser

parser : UrlParser.Parser (Page -> a) a
parser =
    UrlParser.oneOf
        [ UrlParser.map UnDone UrlParser.top
        , UrlParser.map Done (UrlParser.s "done")
        ]

view : Model -> Browser.Document Msg
view model =
    { title = "Elm Todo Tutorial"
    , body =
        [
            Grid.container [] <|
                case model.page of
                    UnDone ->
                        let
                            todos = List.filter (\todo -> not todo.isDone) model.memos
                        in
                            [ CDN.stylesheet
                            , Alert.simpleSecondary [] [
                                h1 [ class "text-center" ] [ text "Todo" ]
                            ]
                            , navBar model
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
                            , table todos
                            ]
                    Done ->
                        let
                            todos = List.filter (\todo -> todo.isDone) model.memos
                        in
                            [ CDN.stylesheet
                            , Alert.simpleSecondary [] [
                                h1 [ class "text-center" ] [ text "Todo" ]
                            ]
                            , navBar model
                            , div [ Spacing.mb3 ] []
                            , table todos
                            ]
                    NotFound ->
                        pageNotFound
        ]
    }

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

navBar : Model -> Html Msg
navBar model =
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.items
            [ Navbar.itemLink [ href "/" ] [ text "UnDone" ]
            , Navbar.itemLink [ href "#done" ] [ text "Done" ]
            ]
        |> Navbar.view model.navbarState

table : List Todo -> Html Msg
table todos =
    Table.table
        { options = [ Table.striped, Table.hover, Table.inversed ]
        , thead = Table.simpleThead
            [ Table.th [] [ text "Number" ]
            , Table.th [] [ text "Task" ]
            , Table.th [] [ text "isDone" ]
            ]
        , tbody = 
            Table.tbody [] (List.map viewMemo todos)
        }

pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not Found" ]
    , text "Sorry coudn't find that page"
    ]