module Codec exposing
    ( languageTo3Code)

import Types exposing (..)

languageTo3Code : Language -> String
languageTo3Code l =
    case l of
        Japanese -> "jpn"
        English -> "eng"

