module Main exposing (main)

import Browser exposing (UrlRequest, Document)
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Url exposing(Url)


main =
    Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = onUrlRequest
    , onUrlChange = onUrlChange
    }

type alias Model =
    { dieFace : Int
    }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( Model 1
    , Cmd.none
    )

onUrlRequest : UrlRequest -> Msg
onUrlRequest _ = NoOp

onUrlChange : Url -> Msg
onUrlChange _ = NoOp

type Msg
    = Roll
    | NewFace Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewFace (Random.int 1 6)
            )

        NewFace newFace ->
            ( Model newFace
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
                [ h1 [] [ text (String.fromInt model.dieFace) ]
                , button [ onClick Roll ] [ text "Roll!" ]
                , text "nya-----------!!!"
                ]
          ]
    }
