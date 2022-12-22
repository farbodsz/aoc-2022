--------------------------------------------------------------------------------

module Day21 (solveA, solveB) where

import Data.Map.Strict ((!))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Debug.Trace (trace)
import Util

--------------------------------------------------------------------------------

type Variable = Text

data BinOp = Add | Sub | Mul | Div
    deriving (Show)

data Expr
    = BinOp BinOp Expr Expr
    | Ref Variable
    | Const Double
    | Unknown
    deriving (Show)

fop :: BinOp -> (Double -> Double -> Double)
fop Add = (+)
fop Sub = (-)
fop Mul = (*)
fop Div = (/)

opposite :: BinOp -> BinOp
opposite Add = Sub
opposite Sub = Add
opposite Mul = Div
opposite Div = Mul

processInput :: Text -> M.Map Variable Expr
processInput = M.fromList . map parseLine . T.lines
  where
    parseLine t = case T.splitOn ": " t of
        [var, op] -> (var, parseOp op)
        _ -> error "parseLine: invalid format"
    parseOp t = case T.words t of
        [x] -> Const (tread x)
        [x, symbol, y] ->
            let op = case symbol of
                    "+" -> Add
                    "-" -> Sub
                    "*" -> Mul
                    "/" -> Div
                    _ -> error "parseOp: unexpected operation"
             in BinOp op (Ref x) (Ref y)
        _ -> error "parseOp: invalid format"

eval :: Expr -> M.Map Variable Expr -> Double
eval (Const x) _ = x
eval (Ref v) store = eval (store ! v) store
eval (BinOp op e1 e2) store = let f = fop op in eval e1 store `f` eval e2 store
eval Unknown _ = error "eval: Unexpected Unknown"

-- | Solves an expression LHS = RHS for one single unknown.
--
-- In summary:
--
-- 1. Ensure the side containing the Unknown is on the RHS.
-- 2. Evaluate/simplify the LHS (which doesn't contain a variable).
-- 3. Simplify by moving part of the RHS without the variable to the LHS.
--
-- Continue this until the RHS is /only/ the Unknown.
--
-- Example: (6 * 2) = (X + (27 / 3)) <=> X = 3
-- @
--    solve  (6 * 2)         (X + (27 / 3))
--    solve  (12)            (X + (27 / 3))    { eval non-variable branch }
--    solve  (12 - (27/3))   (X)               { recursive step }
--    solve  (3)             (X)               { eval non-variable branch }
--    3                                        { base case }
-- @
solve :: Expr -> Expr -> M.Map Variable Expr -> Double
solve lhs rhs store
    | hasUnknown lhs = go rhs lhs
    | hasUnknown rhs = go lhs rhs
    | otherwise = error "solve: One side must have one unknown"
  where
    go :: Expr -> Expr -> Double
    go (Const l) r = trace (show (Const l) ++ " = " ++ show r) $ case r of
        Unknown -> l
        (BinOp op e1 e2) ->
            let revOp = opposite op
             in if hasUnknown e1
                    then go (BinOp revOp (Const l) e2) e1
                    else go (BinOp revOp (Const l) e1) e2
        (Ref v) ->
            trace (" {evaluating variable " ++ show v ++ "}") $
                go (Const l) (store ! v)
        (Const _) -> error "solve: Unexpected Const in RHS"
    go l r =
        trace (show l ++ " = " ++ show r) $
            trace " {evaluating LHS}" $
                go (Const $ eval l store) r

    hasUnknown :: Expr -> Bool
    hasUnknown (BinOp _ x y) = hasUnknown x || hasUnknown y
    hasUnknown (Ref v) = hasUnknown (store ! v)
    hasUnknown (Const _) = False
    hasUnknown Unknown = True

--------------------------------------------------------------------------------

solveA :: Text -> Text
solveA = tshow @Int . round . eval (Ref "root") . processInput

solveB :: Text -> Text
solveB = tshow @Int . round . solveRootParts . modifyInput . processInput
  where
    modifyInput = M.insert "humn" Unknown
    solveRootParts store = case store ! "root" of
        (Const x) -> x
        (BinOp _ l r) -> trace "\nSOLVING:" $ solve l r store
        _ -> error "solveB: Unable to solve for 'root'"

--------------------------------------------------------------------------------
