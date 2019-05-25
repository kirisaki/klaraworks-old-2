module Codec exposing
    ( languageTo3Code
    , codeToLanguage
    , languageDecoder
    )

import Types exposing (..)

import Json.Decode as JD exposing (Decoder)

languageTo3Code : Language -> String
languageTo3Code l =
    case l of
        Japanese -> "jpn"
        English -> "eng"

codeToLanguage : String -> Maybe Language
codeToLanguage str =
    case str of
        "jpn" -> Just Japanese
        "eng" -> Just English
        _ -> Nothing

languageDecoder : Decoder Msg
languageDecoder =
    let
        handler str =
            case codeToLanguage str of
                Just l -> LanguageChanged l
                Nothing -> NoOp
    in
        JD.map handler <| JD.at ["target", "value"] JD.string

