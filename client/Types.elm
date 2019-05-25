module Types exposing
    ( Route(..)
    , Language(..)
    , UrlEvent(..)
    , Msg(..)
    , Model
    , WorkSummary
    , Direction(..)
    , Contents(..)
    , WorkDetail
    )

import Time
import Http
import Url exposing(Url)
import Browser.Navigation exposing (Key)
import Browser exposing (UrlRequest)
import Dict exposing (Dict)

type Route
    = Index
    | About
    | Works (Maybe String)
    | Contact
    | NotFound

type Language
    = Japanese
    | English

type UrlEvent
    = Internal
    | External
    | UrlChanged

type Msg
    = Link UrlEvent Url
    | ReceiveWorksList (Result Http.Error (List WorkSummary))
    | ReceiveWorkDetail (Result Http.Error WorkDetail)
    | LanguageChanged Language
    | Trace String
    | NavPrev
    | NavNext
    | NoOp

type alias Model =
    { key : Key
    , seed : Int
    , route : Route
    , language : Language
    , worksList : Maybe (List WorkSummary)
    , worksDetails : Dict String WorkDetail
    }

type alias WorkSummary =
    { id_ : String
    , time : Time.Posix
    , title : String
     }

type Direction
    = ToRight
    | ToLeft

type Contents
    = Picture String
    | Comic Direction (List String)

type alias WorkDetail =
    { id_ : String
    , time : Time.Posix
    , title : String
    , origin : Maybe String
    , contents : Contents
     }

