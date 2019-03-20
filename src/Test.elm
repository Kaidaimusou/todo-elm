import Bootstrap.Navbar as Navbar
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)

main : Program () Model Msg
main =
    Browser.element
        { init = initialState
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


-- You need to keep track of the view state for the navbar in your model

type alias Model =
    { navbarState : Navbar.State }


-- The navbar needs to know the initial window size, so the inital state for a navbar requires a command to be run by the Elm runtime

initialState : flags -> ( Model, Cmd Msg )
initialState _ =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
        ( { navbarState = navbarState }, navbarCmd )


-- Define a message for the navbar

type Msg
    = NavbarMsg Navbar.State


-- You need to handle navbar messages in your update function to step the navbar state forward

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Navbar.config NavbarMsg
            |> Navbar.withAnimation
            |> Navbar.brand [ href "#" ] [ text "Brand" ]
            |> Navbar.items
                [ Navbar.itemLink [ href "/src/Test.elm/item1" ] [ text "Item 1" ]
                , Navbar.itemLink [ href "/src/Test.el/item2" ] [ text "Item 2" ]
                ]
            |> Navbar.view model.navbarState
        ]


-- If you use animations as above or you use dropdowns in your navbar you need to configure subscriptions too

subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navbarState NavbarMsg