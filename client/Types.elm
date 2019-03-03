module Types exposing
    ( Route(..)
    , Language(..)
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
    , contets : Contents
     }

