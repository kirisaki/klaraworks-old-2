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
import Json.Decode as JD
import Browser.Events

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

toUrl : Route -> String
toUrl r =
    case r of
        Index ->
            "/"
        About ->
            "/about"
        Works Nothing ->
            "/works"
        Works (Just x) ->
            "/works/" ++ x
        Contact ->
            "/contact"
        NotFound ->
            "/404"

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
            ( { model | language = l }
            , Task.attempt ReceiveWorksList (Fetch.worksList l)
            )

        NavPrev ->
            let
                newRoute =
                    case model.route of
                        Index ->
                            Index
                        About ->
                            Index
                        Works Nothing ->
                            About
                        Works (Just x) ->
                            Works (Just x)
                        Contact ->
                            Works Nothing
                        NotFound->
                            Index
                newUrl = toUrl newRoute
            in
                ( { model | route = newRoute }
                , Nav.pushUrl model.key newUrl
                )
        NavNext ->
            let
                newRoute =
                    case model.route of
                        Index ->
                            About
                        About ->
                            Works Nothing
                        Works Nothing ->
                            Contact
                        Works (Just x) ->
                            Works (Just x)
                        Contact ->
                            Contact
                        NotFound->
                            Index
                newUrl = toUrl newRoute
            in
                ( { model | route = newRoute }
                , Nav.pushUrl model.key newUrl
                )
        NoOp ->
            ( model
            , Cmd.none
            )
        Trace str ->
            ( Debug.log str model, Cmd.none )


keyHandler : Sub Msg
keyHandler =
    let
        toMsg str =
            case str of
                "ArrowLeft" -> NavPrev
                "ArrowRight" -> NavNext
                "h" -> NavPrev
                "l" -> NavNext
                _ -> NoOp

        decoder =
            JD.map toMsg (JD.field "key" JD.string)
    in
    Browser.Events.onKeyDown decoder


subscriptions : Model -> Sub Msg
subscriptions _ =
    keyHandler



