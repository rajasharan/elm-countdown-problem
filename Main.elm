import Html exposing (beginnerProgram)

import Types exposing (..)
import Views exposing (..)
import Utils exposing (..)

main =
  beginnerProgram { model = model, view = view, update = update }

model = 0

-- UPDATE

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

