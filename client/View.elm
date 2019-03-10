module View exposing (view)

import Types exposing(..)
import Codec

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Json.Decode as JD

view : Model -> Document Msg
view model =
    { title = "Klara Works"
    , body =
          [ div [ classList
                      [ ( "container", True )
                      , ( case model.route of
                              Index -> "index"
                              About -> "about"
                              Works _ -> "works"
                              Contact -> "contact"
                              _ -> ""
                        , True )
                      ]
                ]
                [ index model
                , about model
                , works model
                , contact model
                ]
          , navigation model
          , setting model
          ]
    }

navigation : Model -> Html Msg
navigation model =
    nav []
    [ ul []
      [ a [ href "/" ] [ li [] [ text "index" ] ]
      , a [ href "/about" ] [ li [] [ text "about" ] ]
      , a [ href "/works" ] [ li [] [ text "works" ] ]
      , a [ href "/contact" ] [ li [] [ text "contact" ] ]
      ]
    ]

setting : Model -> Html Msg
setting model =
    ul [ class "setting" ]
    [ li [][ div []
                 [ text "language: "
                 , select [ class "language_selector",  on "change" Codec.languageDecoder ]
                     [ option [ value "jpn", selected (model.language == Japanese) ] [ text "日本語" ]
                     , option [ value "eng", selected (model.language == English) ] [ text "English" ]
                     ]
                 ] ]
    , li [][ div []
                 [ text "seed: "
                 , input [ class "seed", type_ "text", size 11, maxlength 11, value <| String.fromInt model.seed ][]
                 ] ]
    ]

index : Model -> Html Msg
index model = div [ class "index" ]
              [ h1 [] [ span [] [ text "Klara Works" ]  ] ]

about : Model -> Html Msg
about model = div []
              [ text "about"
              ]

works : Model -> Html Msg
works model = div []
              [ text "works"
              ]

contact : Model -> Html Msg
contact model = div []
                [ text "contact"
                ]

notFound : Model -> Html Msg
notFound model = div []
                 [ text "nyaan..."
                 ]
