module Types exposing (..)

type Op = Add | Sub | Mul | Div
type Expr = Val Int | App Op Expr Expr

type alias Res = (Expr, Int)
type alias Model = Int

type Msg = Increment | Decrement
