module Main exposing (main)

import Browser exposing (UrlRequest, Document)
import Browser.Navigation as Nav exposing( Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Url exposing(Url)
import Url.Parser as UP exposing((</>),Parser)


main =
    Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = Link
    , onUrlChange = UrlChanged
    }


type Route
    = Index
    | About
    | Works (Maybe String)
    | Contact
    | NotFound

type Language
    = Japanese
    | English

type Msg
    = Link UrlRequest
    | UrlChanged Url
    | NoOp

type alias Model =
    { key : Key
    , route : Route
    , language : Language
    }

init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ _ k =
    ( { key = k
      , route = Index
      , language = Japanese
      }
    , Cmd.none
    )

router : Url -> Route
router url =
    let
        parser =
            UP.oneOf
                 [ UP.map Index UP.top
                 , UP.map About (UP.s "about")
                 , UP.map (Works Nothing) (UP.s "works")
                 , UP.map (Works << Just) (UP.s "works" </> UP.string)
                 , UP.map Contact (UP.s "contact")
                 ]
    in
        case UP.parse parser url of
            Just r -> r
            _ -> NotFound

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Link req ->
            case req of
                Browser.Internal url ->
                    ( { model | route = router url }
                    , Nav.pushUrl model.key (Url.toString url)
                    )
                Browser.External url ->
                    ( model
                    , Nav.load url
                    )
        UrlChanged url ->
            ( { model | route = router url }
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "Klara Works"
    , body =
          [ div [ class "container" ]
                [ case model.route of
                      Index -> index model
                      About -> about model
                      Works Nothing -> works model
                      Works _ -> notFound model
                      Contact -> contact model
                      NotFound -> notFound model
                , a [ href "/" ] [ text "index" ]
                , a [ href "/about" ] [ text "about" ]
                , a [ href "/works" ] [ text "works" ]
                , a [ href "/contact" ] [ text "contact" ]
                ]
          ]
    }

index : Model -> Html Msg
index model = text "index"

about : Model -> Html Msg
about model = text "about"

works : Model -> Html Msg
works model = div []
              [ a [ href "/works/1" ] [ text "1" ]
              , a [ href "/works/2" ] [ text "2" ]
              ]

contact : Model -> Html Msg
contact model = text "contact"



notFound : Model -> Html Msg
notFound model = text "nyaan..."
