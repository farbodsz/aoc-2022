--------------------------------------------------------------------------------

module Day21 (solveA, solveB) where

import Data.Map.Strict ((!))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Util

--------------------------------------------------------------------------------

type Variable = Text

data Expr
    = Add Expr Expr
    | Subtract Expr Expr
    | Multiply Expr Expr
    | Divide Expr Expr
    | Ref Variable
    | Const Int

processInput :: Text -> M.Map Variable Expr
processInput = M.fromList . map parseLine . T.lines
  where
    parseLine t = case T.splitOn ": " t of
        [var, op] -> (var, parseOp op)
        _ -> error "parseLine: invalid format"
    parseOp t = case T.words t of
        [x] -> Const (tread x)
        [x, "+", y] -> Add (Ref x) (Ref y)
        [x, "-", y] -> Subtract (Ref x) (Ref y)
        [x, "*", y] -> Multiply (Ref x) (Ref y)
        [x, "/", y] -> Divide (Ref x) (Ref y)
        _ -> error "parseOp: invalid format"

eval :: Expr -> M.Map Variable Expr -> Int
eval (Const x) _ = x
eval (Ref v) store = eval (store ! v) store
eval (Add e1 e2) store = eval e1 store + eval e2 store
eval (Subtract e1 e2) store = eval e1 store - eval e2 store
eval (Multiply e1 e2) store = eval e1 store * eval e2 store
eval (Divide e1 e2) store = eval e1 store `div` eval e2 store

--------------------------------------------------------------------------------

solveA :: Text -> Text
solveA = tshow . eval (Ref "root") . processInput

solveB :: Text -> Text
solveB = undefined

--------------------------------------------------------------------------------
