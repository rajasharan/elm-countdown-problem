module Types exposing (..)

type Op = Add | Sub | Mul | Div
type Expr = Val Int | App Op Expr Expr

type alias Res = (Expr, Int)
type alias Model =
  { input1 : Int
  , input2 : Int
  , input3 : Int
  , input4 : Int
  , input5 : Int
  , input6 : Int
  , target : Int
  , result : List String
  }

type Msg =
    Input1 String
  | Input2 String
  | Input3 String
  | Input4 String
  | Input5 String
  | Input6 String
  | Target String
  | Evaluate


