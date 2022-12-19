--------------------------------------------------------------------------------

module Day18 (solveA, solveB) where

import Data.Map ((!))
import Data.Map.Strict qualified as M
import Data.Set ((\\))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Util

--------------------------------------------------------------------------------

type Pos = (Int, Int, Int)

processInput :: Text -> S.Set Pos
processInput = S.fromList . map processLine . T.lines
  where
    processLine t = case T.splitOn "," t of
        [x, y, z] -> (tread x, tread y, tread z)
        _ -> error "processLine: parse error"

touching :: Pos -> S.Set Pos
touching p@(x, y, z) =
    S.filter (\(x', y', z') -> twoTrue [x' == x, y' == y, z' == z]) $ adjacent p
  where
    twoTrue = (== 2) . length . filter (== True)

adjacent :: Pos -> S.Set Pos
adjacent p@(x, y, z) =
    S.filter (/= p) . S.fromList $
        [(x', y', z') | x' <- surround x, y' <- surround y, z' <- surround z]
  where
    surround n = [n - 1 .. n + 1]

cubeSides :: Int
cubeSides = 6

countSides :: S.Set Pos -> Int
countSides ps = sum $ map (\p -> cubeSides - countTouching p) (S.toList ps)
  where
    countTouching p = S.size $ S.intersection (touching p) (S.delete p ps)

--------------------------------------------------------------------------------
-- Solutions

solveA :: Text -> Text
solveA = tshow . countSides . processInput

solveB :: Text -> Text
solveB = undefined

--------------------------------------------------------------------------------
