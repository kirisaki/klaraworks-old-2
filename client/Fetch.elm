module Fetch exposing
    ( worksList
    , workDetail
    )
import Types exposing(..)
import Codec

import Http
import Time
import Bytes
import Bytes.Decode as BD
import Task exposing (..)


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

list16 : BD.Decoder a -> BD.Decoder (List a)
list16 d =
    let
        listStep d1 (n, xs) =
            if n <= 0 then
                BD.succeed (BD.Done xs)
            else
                BD.map (\x -> BD.Loop (n - 1, x :: xs)) d1
    in
        BD.unsignedInt16 Bytes.BE
            |> BD.andThen (\len -> BD.loop (len, []) (listStep d))

worksList : Language -> Task Http.Error (List WorkSummary)
worksList l =
    let
        summary = BD.map3 WorkSummary string8 posix string16
        decoder = apiDecoder <| list16 summary
    in
        Http.task
            { method = "GET"
            , headers = []
            , url = "/api/works?" ++ Codec.languageTo3Code l
            , body = Http.emptyBody
            , resolver = bytesResolver decoder
            , timeout = Nothing
            }

workDetail : String -> Language -> Task Http.Error WorkDetail
workDetail i l =
    let
        origin =
            string8
                |> BD.andThen
                   (\str ->
                        case str of
                            "" -> BD.succeed Nothing
                            x -> BD.succeed <| Just x
                   )
        contents =
            BD.unsignedInt8
                |> BD.andThen
                   (\t ->
                       case t of
                           0x01 ->
                               BD.map Picture string8
                           0x02 ->
                               BD.map (Comic ToRight) (list16 string8)
                           0x03 ->
                               BD.map (Comic ToLeft) (list16 string8)
                           _ -> BD.fail
                   )
        detail =
            BD.map5 WorkDetail
                string8
                posix
                string16
                origin
                contents
        decoder = apiDecoder <| detail
    in
        Http.task
            { method = "GET"
            , headers = []
            , url = "/api/works/" ++ i ++ "?" ++ Codec.languageTo3Code l
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
