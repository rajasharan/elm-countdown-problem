import Html exposing (beginnerProgram)
import Debug exposing (log)

import Types exposing (..)
import Views exposing (..)
import Utils exposing (..)

main =
  beginnerProgram { model = model, view = view, update = update }

model =
  { input1 = 1
  , input2 = 3
  , input3 = 7
  , input4 = 10
  , input5 = 25
  , input6 = 50
  , target = 765
  , result = []
  }

-- UPDATE

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input1 x -> { model | input1 = toInt x model.input1 }
    Input2 x -> { model | input2 = toInt x model.input2 }
    Input3 x -> { model | input3 = toInt x model.input3 }
    Input4 x -> { model | input4 = toInt x model.input4 }
    Input5 x -> { model | input5 = toInt x model.input5 }
    Input6 x -> { model | input6 = toInt x model.input6 }
    Target x -> { model | target = toInt x model.target }
    Evaluate -> { model | result = evaluate model }

toInt : String -> Int -> Int
toInt str default = String.toInt str |> Result.toMaybe |> Maybe.withDefault default

evaluate : Model -> List String
evaluate model = List.map print <| solutionsR [model.input1, model.input2, model.input3, model.input4, model.input5, model.input6] model.target
