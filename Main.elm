import Html exposing (program)
import Debug exposing (log)
import Task exposing (succeed, perform)
import Time exposing (millisecond)
import Process exposing (sleep)
import List exposing (length)

import Types exposing (..)
import Views exposing (..)
import Utils exposing (..)

main =
  program { init = init, view = view, update = update, subscriptions = subscriptions }

init = model ! []

model =
  { input1 = 1
  , input2 = 3
  , input3 = 7
  , input4 = 10
  , input5 = 25
  , input6 = 50
  , target = 765
  , isLoading = False
  , results = [""]
  , error = ""
  }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Input1 x        -> { model | input1 = toInt x model.input1 }  ! []
    Input2 x        -> { model | input2 = toInt x model.input2 }  ! []
    Input3 x        -> { model | input3 = toInt x model.input3 }  ! []
    Input4 x        -> { model | input4 = toInt x model.input4 }  ! []
    Input5 x        -> { model | input5 = toInt x model.input5 }  ! []
    Input6 x        -> { model | input6 = toInt x model.input6 }  ! []
    Target x        -> { model | target = toInt x model.target }  ! []
    Evaluate        -> { model | isLoading = True }               ! [perform Delay <| sleep (millisecond * 500)]
    Delay _         -> model                                      ! [perform StartLoading <| succeed ()]
    StartLoading _  -> ( model |> evaluate |> setError )          ! [perform StopLoading <| succeed ()]
    StopLoading _   -> { model | isLoading = False }              ! []

toInt : String -> Int -> Int
toInt str default = String.toInt str |> Result.toMaybe |> Maybe.withDefault default

evaluate : Model -> Model
evaluate model = { model | results = List.map print <| solutionsR [model.input1, model.input2, model.input3, model.input4, model.input5, model.input6] model.target }

setError : Model -> Model
setError model = { model | error = if length model.results <= 0 then "No valid expressions found for target: " ++ toString model.target else "" }
