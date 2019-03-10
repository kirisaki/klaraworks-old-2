module Main exposing (main)

import Types exposing (..)
import Fetch
import View exposing (view)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing( Key)
import Random
import Url exposing(Url)
import Url.Parser as UP exposing((</>),Parser)
import Task
import Dict exposing (Dict)

main =
    Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = onUrlRequest
    , onUrlChange = Link UrlChanged
    }


onUrlRequest : UrlRequest -> Msg
onUrlRequest req =
    case req of
        Browser.Internal url ->
            Link Internal url
        Browser.External str ->
            case Url.fromString str of
                Just url -> Link External url
                _ -> NoOp

init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url k =
    let
        initRoute = router url
        initCmd =
            case initRoute of
                Works Nothing ->
                    Task.attempt ReceiveWorksList (Fetch.worksList Japanese)
                Works (Just i) ->
                    Task.attempt ReceiveWorkDetail (Fetch.workDetail i Japanese)
                _ ->
                    Cmd.none
    in
        ( { key = k
          , seed = 1234567890
          , route = router url
          , language = Japanese
          , worksList = Nothing
          , worksDetails = Dict.empty
          }
        , initCmd
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
        Link event url ->
            let
                newRoute = router url
                newModel = { model | route = newRoute }
                newCmd =
                    case (event, newRoute) of
                        (Internal, Works Nothing) ->
                            Cmd.batch
                                [ Task.attempt ReceiveWorksList (Fetch.worksList model.language)
                                , Nav.pushUrl model.key (Url.toString url)
                                ]
                        (Internal, Works (Just i)) ->
                            Cmd.batch
                                [ Task.attempt ReceiveWorkDetail (Fetch.workDetail i model.language)
                                , Nav.pushUrl model.key (Url.toString url)
                                ]
                        (Internal, _) ->
                                Nav.pushUrl model.key (Url.toString url)
                        (External, _) ->
                            Nav.load (Url.toString url)
                        (UrlChanged, _) ->
                            Cmd.none
            in
                (newModel, newCmd)
        ReceiveWorksList res ->
            case res of
                Ok ws ->
                    ( { model | worksList = Just ws }
                    , Cmd.none
                    )
                Err _ ->
                    ( model
                    , Cmd.none
                    )
        ReceiveWorkDetail res ->
            case res of
                Ok wd ->
                    ( { model | worksDetails = Dict.insert wd.id_ wd model.worksDetails }
                    , Cmd.none
                    )
                Err _ ->
                    ( model
                    , Cmd.none
                    )
        LanguageChanged l ->
            ( { model | language = l }, Cmd.none )

        Trace str ->
            ( Debug.log str model, Cmd.none )
        NoOp ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


