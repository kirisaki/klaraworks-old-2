module Main exposing (main)

import Browser exposing (UrlRequest, Document)
import Browser.Navigation as Nav exposing( Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Url exposing(Url)
import Url.Parser as UP exposing((</>),Parser)
import Http
import Time
import Bytes
import Bytes.Decode as BD
import Task exposing (..)

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
    | ReceiveWorksList (Result Http.Error (List WorkSummary))
    | NoOp

type alias Model =
    { key : Key
    , route : Route
    , language : Language
    , worksList : Maybe (List WorkSummary)
    }

type alias WorkSummary =
    { id_ : String
    , time : Time.Posix
    , title : String
     }

init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ _ k =
    ( { key = k
      , route = Index
      , language = Japanese
      , worksList = Nothing
      }
    , Task.attempt ReceiveWorksList (fetchWorksList Japanese)
    )

languageTo3Code : Language -> String
languageTo3Code l =
    case l of
        Japanese -> "jpn"
        English -> "eng"


apiDecoder : BD.Decoder a -> BD.Decoder a
apiDecoder d =
    BD.unsignedInt8 |> BD.andThen
        (\status ->
            case status of
                0x22 -> d
                _ -> BD.fail
        )

string8 : BD.Decoder String
string8 =
  BD.unsignedInt8
    |> BD.andThen BD.string

string16 : BD.Decoder String
string16 =
  BD.unsignedInt16 Bytes.BE
    |> BD.andThen BD.string

posix : BD.Decoder Time.Posix
posix = BD.map (Time.millisToPosix << ((*) 1000)) <| BD.signedInt32 Bytes.BE

fetchWorksList : Language -> Task Http.Error (List WorkSummary)
fetchWorksList l =
    let
        list d =
            BD.unsignedInt16 Bytes.BE
                |> BD.andThen (\len -> BD.loop (len, []) (listStep d))
        listStep d (n, xs) =
            if n <= 0 then
                BD.succeed (BD.Done xs)
            else
                BD.map (\x -> BD.Loop (n - 1, x :: xs)) d
        summary = BD.map3 WorkSummary string8 posix string16
        decoder = apiDecoder <| list summary
    in
        Http.task
            { method = "GET"
            , headers = []
            , url = "/api/works?" ++ languageTo3Code l
            , body = Http.emptyBody
            , resolver = bytesResolver decoder
            , timeout = Nothing
            }

bytesResolver : BD.Decoder a -> Http.Resolver Http.Error a
bytesResolver decoder =
    Http.bytesResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case BD.decode decoder body of
                        Just value ->
                            Ok value

                        Nothing ->
                            Err (Http.BadBody "fail parsing")
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
        ReceiveWorksList res ->
            case Debug.log "res" res of
                Ok ws ->
                    ( { model | worksList = Just ws }
                    , Cmd.none
                    )
                Err _ ->
                    ( model
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
works model =
    case model.worksList of
        Just ws ->
            div [] (List.map (\s -> h1 [] [ text s.title ]) ws)
        _ ->
            div [] [ text "nyaan!!!" ]


contact : Model -> Html Msg
contact model = text "contact"



notFound : Model -> Html Msg
notFound model = text "nyaan..."
