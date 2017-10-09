module Utils exposing (..)

import Maybe exposing (withDefault)
import List exposing (concatMap, range, length, head, isEmpty, concat, filter, map, map2, member)
import List.Extra exposing (andThen, splitAt, permutations, subsequences)

import Types exposing (..)

valid : Op -> Int -> Int -> Bool
valid op x y =
  case (op, x, y) of
    (Add, x, y) -> x <= y
    (Sub, x, y) -> x > y
    (Mul, x, y) -> not (x == 1) && not (y == 1) && x <= y
    (Div, x, y) -> not (y == 1) && rem x y == 0

valid_ : Op -> (Int, Int) -> Bool
valid_ op xy =
  case xy of
    (x, y) -> valid op x y

apply : Op -> Int -> Int -> Int
apply op x y =
  case (op, x, y) of
    (Add, x, y) -> x + y
    (Sub, x, y) -> x - y
    (Mul, x, y) -> x * y
    (Div, x, y) -> x//y

apply_ : Op -> (Int, Int) -> Int
apply_ op xy =
  case xy of
    (x, y) -> apply op x y

values : Expr -> List Int
values expr =
  case expr of
    Val n -> [n]
    App _ l r -> values l ++ values r

eval : Expr -> List Int
eval expr =
  case expr of
    Val n -> filter (\n -> n > 0) [n]
    App o l r ->
      let
          lr_ = map2 (,) (eval l) (eval r)
          lr = filter (uncurry (valid o)) lr_
      in
          map (uncurry (apply o)) lr

subsets : List a -> List (List a)
subsets xs =
  case xs of
    [] -> [[]]
    xs -> concat <| map permutations (subsequences xs)

solution : Expr -> List Int -> Int -> Bool
solution e ns n = member (values e) (subsets ns) && eval e == [n]

splitN : List Int -> List (List Int, List Int)
splitN ns =
  case ns of
    [] -> []
    [x] -> []
    [x, y] -> [([x], [y])]
    xs ->
      let
          cs = range 1 (length ns - 1)
          fs = map (\n -> splitAt n) cs
      in
          map (\f -> f xs) fs

nesplitN : List Int -> List (List Int, List Int)
nesplitN ns = filter ne (splitN ns)

split : List a -> List (List a, List a)
split xs =
  case xs of
    [] -> [([], [])]
    (x :: xs) -> ([], x :: xs) :: map (\(l,r) -> (x :: l, r)) (split xs)

nesplit : List a -> List (List a, List a)
nesplit xs = filter ne (split xs)

ne : (List a, List b) -> Bool
ne (xs, ys) = not (isEmpty xs || isEmpty ys)

exprs : List Int -> List Expr
exprs xs =
  case xs of
    [] -> []
    [n] -> [Val n]
    ns ->
      nesplitN ns |> andThen (\(l,r) -> exprs l
                  |> andThen (\l -> exprs r
                  |> andThen (\r -> combine l r)))

combine : Expr -> Expr -> List Expr
combine l r = map (app l r) ops

results : List Int -> List Res
results ns =
  case ns of
    [] -> []
    --[n] -> filter (\(e,n) -> n > 0) [(Val n, n)]
    [n] -> [(Val n, n)] |> andThen (\(e,n) -> if n > 0 then [(e,n)] else [])
    ns ->
      nesplitN ns
        |> andThen (\(ls,rs) -> results ls
        |> andThen (\lx -> results rs
        |> andThen (\rx -> combineR lx rx)))

combineR : Res -> Res -> List Res
combineR ls rs =
  case (ls, rs) of
    ((l,x),(r,y)) -> map (\(o,x,y) -> (App o l r, apply o x y)) <| filter (\(o,x,y) -> valid o x y) <| map (\o -> (o,x,y)) ops

app : Expr -> Expr -> Op -> Expr
app l r o = App o l r

ops : List Op
ops = [Add, Sub, Mul, Div]

--solutions : List Int -> Int -> List Expr
--solutions ns n =
  --filter (\e -> eval e == [n]) (concatMap exprs (subsets ns))

solutionsR : List Int -> Int -> List Expr
solutionsR ns n =
  subsets ns
    |> andThen (\nss -> results nss
    |> andThen (\(e,m) -> if m == n then [e] else []))

inputs = [1, 3, 7, 10, 25, 50]
targetValue = 765
--find = solutions inputs
findR = solutionsR inputs

input_ = [1, 50, 25, 10]
target_ = 765

print : Expr -> String
print expr =
    case expr of
        Val n -> toString n
        App o l r ->
            case o of
                Add -> "(" ++ print l ++ " + " ++ print r ++ ")"
                Sub -> "(" ++ print l ++ " - " ++ print r ++ ")"
                Mul -> "(" ++ print l ++ " * " ++ print r ++ ")"
                Div -> "(" ++ print l ++ " / " ++ print r ++ ")"

