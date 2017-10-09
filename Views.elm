module Views exposing (..)

import Html exposing (Html, button, div, text, section, h1, ul, li, strong, span, a, i, input, pre, code)
import Html.Attributes exposing (class, target, href, title, placeholder, type_, value)
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import List exposing (concatMap, range, length, head, isEmpty, concat, filter, map, map2, member)
import List.Extra exposing (andThen, splitAt, permutations, subsequences)

import Types exposing (..)
import Utils exposing (..)

view : Model -> Html Msg
view model =
  section []
    [ header model, userinput model, evaluateSection model, resultsection model]

header : Model -> Html Msg
header model =
  section [class "hero is-primary"]
    [ div [class "hero-body"]
      [ div [class "container"]
        [ h1 [class "title"] [text "The Countdown Problem"]
        , div [ class "subtitle content" ]
          [ ul []
            [ li []
              [ text "Pick 6 numbers greater than 0" ]
            , li []
              [ text "Pick a target numbers greater than 0" ]
            , li []
              [ text "Find all expressions that lead to target using operators "
              , strong []
                [ text "+ - x /" ]
              ]
            , li []
              [ text "All sub-expressions should be numbers greater than 0" ]
            , li []
              [ text "Inspired by ", strong [] [text "Graham Huttons "], text "paper "
              , span [ class "icon" ]
                [ a [ href "http://www.cs.nott.ac.uk/~pszgmh/countdown.pdf", target "_blank", title "The countdown problem pdf paper" ]
                  [i [ class "fa fa-external-link" ] []]
                ]
              ]
            ]
          ]
        ]
      ]
    ]

userinput : Model -> Html Msg
userinput model =
  section []
    [ div [ class "notification" ]
      [ div [ class "notification" ]
        [ text "Enter numbers " , strong [] [text "greater than 0 "] , text "below.            "
        , div [class "columns"]
          [ div [class "column is-narrow is-2"]
            [ div [class "field"]
              [ div [class "control"]
                [ input [class "input is-large", placeholder ">0", value "1", type_ "text"]
                  []
                ]
              ]
            ]

          , div [ class "column is-narrow is-2" ]
            [ div [ class "field" ]
              [ div [ class "control" ]
                [ input [ class "input is-large", placeholder ">0", type_ "text", value "3" ]
                  []
                ]
              ]
            ]

          , div [ class "column is-narrow is-2" ]
            [ div [ class "field" ]
              [ div [ class "control" ]
                [ input [ class "input is-large", placeholder ">0", type_ "text", value "7" ]
                  []
                ]
              ]
            ]

          , div [ class "column is-narrow is-2" ]
            [ div [ class "field" ]
              [ div [ class "control" ]
                [ input [ class "input is-large", placeholder ">0", type_ "text", value "10" ]
                  []
                ]
              ]
            ]

          , div [ class "column is-narrow is-2" ]
            [ div [ class "field" ]
              [ div [ class "control" ]
                [ input [ class "input is-large", placeholder ">0", type_ "text", value "25" ]
                  []
                ]
              ]
            ]

          , div [ class "column is-narrow is-2" ]
            [ div [ class "field" ]
              [ div [ class "control" ]
                [ input [ class "input is-large", placeholder ">0", type_ "text", value "50" ]
                  []
                ]
              ]
            ]
          ]
        ]
      ]
    ]

evaluateSection : Model -> Html Msg
evaluateSection model =
  section []
    [ div [ class "columns notification" ]
      [ div [ class "column" ]
        [ div [ class "notification" ]
          [ text "Enter target num " , strong [] [ text "greater than 0 " ] , text "below."
          , div [ class "field has-addons" ]
            [ div [ class "control" ]
              [ a [ class "button is-info is-large is-info" ]
                [ text "Evaluate" ]
              ]
            , div [ class "control" ]
              [ input [ class "input is-large", placeholder "target #", type_ "text", value "765" ]
                []
              ]
            ]
          ]
        ]
      ]
    ]

resultsection : Model -> Html Msg
resultsection model =
  let
      results = map print <| findR 76
  in
      section [] <| map (precode model) results
        
precode : Model -> String -> Html Msg
precode model str = pre [] [ code [] [text str] ]
