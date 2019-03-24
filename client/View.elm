module View exposing (view)

import Types exposing(..)
import Codec

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy)
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
                [ lazy index model
                , lazy about model
                , lazy works model
                , lazy contact model
                ]
          , lazy navigation model
          , lazy setting model
          ]
    }

navigation : Model -> Html Msg
navigation model =
    nav []
    [ ul []
      [ a [ href "/", classList [ ( "active", model.route == Index ) ] ] [ li [] [ text "index" ] ]
      , a [ href "/about", classList [ ( "active", model.route == About ) ] ] [ li [] [ text "about" ] ]
      , a [ href "/works", classList [ ( "active"
                                       , case model.route of
                                             Works _ -> True
                                             _ -> False)
                                     ] ] [ li [] [ text "works" ] ]
      , a [ href "/contact", classList [ ( "active", model.route == Contact ) ] ] [ li [] [ text "contact" ] ]
      ]
    ]

setting : Model -> Html Msg
setting model =
    ul [ class "setting" ]
    [ li [][ div []
                 [ label [ for "language" ] [ text "language: " ]
                 , select [ class "language_selector", id "language",  on "change" Codec.languageDecoder ]
                     [ option [ value "jpn", selected (model.language == Japanese) ] [ text "日本語" ]
                     , option [ value "eng", selected (model.language == English) ] [ text "English" ]
                     ]
                 ] ]
    , li [][ div []
                 [ label [ for "seed" ] [ text "seed: " ]
                 , input [ class "seed", id "seed", type_ "text", size 11, maxlength 11, value <| String.fromInt model.seed ][]
                 ] ]
    ]

index : Model -> Html Msg
index model = div [ class "index" ]
              [ h1 [] [ span [] [ text "Klara Works" ]  ] ]

about : Model -> Html Msg
about model = div [ class "about" ] [
                article []
                    [ introduction model.language
                    , introduction model.language
                    , introduction model.language
                    , introduction model.language
                    , introduction model.language
                    , introduction model.language
                    ]
              ]

card : String -> Html Msg -> Html Msg
card title content = div [] [ section []
                     [ h1 []  [ text title ]
                     , p [] [ content ]
                     ] ]

introduction : Language -> Html Msg
introduction l =
    card "Introduction" <| text <|
        case l of
            Japanese -> """
                         サークル「Klara Works（クラーラ･ワークス）」 は霧咲空人（きりさきあきひと）の運営する個人の同人サークルです。
                         主にイラスト・漫画の制作を行い同人誌として即売会で頒布したり、ウェブ上で作品を発表しています。
                         このサイトではイベントの参加情報や外部サービスへのリンク、同人誌情報の掲載などを行っています。
                         """
            English -> """
                        Circle "Klara Works" is a doujin circle run by Akihito KIRISAKI.
                        I mainly produce illustrations, mangas, distribute it as an doujinshi in doujin events,
                        and present works on the web.
                        This site provides participation information of the doujin event,
                        links to external web services, publication of doujinshi information etc. are carried out.
                        """
               

works : Model -> Html Msg
works model = div [ class "works" ]
              [ article []
                    (case model.worksList of
                         Just l ->
                             List.map
                                 (\r ->
                                      div [] [section [] [ text r.title ]]
                                 ) l
                         Nothing ->
                             [ text "nyaan..." ]
                    )
              ]
contact : Model -> Html Msg
contact model = div []
                [ text "contact"
                ]

notFound : Model -> Html Msg
notFound model = div []
                 [ text "nyaan..."
                 ]
