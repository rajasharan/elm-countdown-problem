module Views exposing (..)

import Html exposing (Html, button, div, text, section, h1, ul, li, strong, span, a, i, input, pre, code)
import Html.Attributes exposing (class, target, href, title, placeholder, type_, value, disabled)
import Html.Events exposing (onClick, onInput)
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
  section [class "hero is-primary is-bold"]
    [ div [class "hero-body"]
      [ div [class "container"]
        [ h1 [class "title"] [text "The Countdown Problem"]
        , div [ class "subtitle content" ]
          [ ul []
            [ li []
              [ text "Pick 6 numbers greater than 0" ]
            , li []
              [ text "Pick a target number greater than 0" ]
            , li []
              [ text "Find all expressions that lead to target using operators "
              , strong []
                [ text "+ - x /" ]
              ]
            , li []
              [ text "All sub-expressions should be numbers greater than 0" ]
            , li []
              [ text "Inspired by ", strong [] [text "Graham Hutton's "], text "paper "
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
        [ text "Enter numbers " , strong [] [text "greater than 0 "] , text "below."
        , div [class "columns"]
          [ div [class "column is-narrow is-2"]
            [ div [class "field"]
              [ div [class "control"]
                [ input [class "input is-large", placeholder ">0", type_ "number", value <| toString model.input1, onInput Input1]
                  []
                ]
              ]
            ]

          , div [ class "column is-narrow is-2" ]
            [ div [ class "field" ]
              [ div [ class "control" ]
                [ input [ class "input is-large", placeholder ">0", type_ "number", value <| toString model.input2, onInput Input2 ]
                  []
                ]
              ]
            ]

          , div [ class "column is-narrow is-2" ]
            [ div [ class "field" ]
              [ div [ class "control" ]
                [ input [ class "input is-large", placeholder ">0", type_ "number", value <| toString model.input3, onInput Input3 ]
                  []
                ]
              ]
            ]

          , div [ class "column is-narrow is-2" ]
            [ div [ class "field" ]
              [ div [ class "control" ]
                [ input [ class "input is-large", placeholder ">0", type_ "number", value <| toString model.input4, onInput Input4 ]
                  []
                ]
              ]
            ]

          , div [ class "column is-narrow is-2" ]
            [ div [ class "field" ]
              [ div [ class "control" ]
                [ input [ class "input is-large", placeholder ">0", type_ "number", value <| toString model.input5, onInput Input5 ]
                  []
                ]
              ]
            ]

          , div [ class "column is-narrow is-2" ]
            [ div [ class "field" ]
              [ div [ class "control" ]
                [ input [ class "input is-large", placeholder ">0", type_ "number", value <| toString model.input6, onInput Input6 ]
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
  let
      isLoading = if model.isLoading then " is-loading is-warning" else " is-info"
  in
  section []
    [ div [ class "columns notification" ]
      [ div [ class "column" ]
        [ div [ class "notification" ]
          [ text "Enter target num " , strong [] [ text "greater than 0 " ] , text "below."
          , div [ class "field has-addons" ]
            [ div [ class "control" ]
              [ a [ class <| "button is-large" ++ isLoading, onClick Evaluate, disabled model.isLoading ]
                [ text "Evaluate" ]
              ]
            , div [ class "control" ]
              [ input [ class "input is-large", placeholder "target #", type_ "number", value <| toString model.target, onInput Target]
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
      results = if String.length model.error > 0 then [errorsection model] else map precode model.results
  in
      section [] results
        
precode : String -> Html Msg
precode str = pre [] [ code [] [text str] ]

errorsection : Model -> Html Msg
errorsection model =
  div [ class "notification is-danger title content" ] [ text model.error ]
