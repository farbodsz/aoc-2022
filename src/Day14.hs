--------------------------------------------------------------------------------

module Day14 (solveA, solveB) where

import Control.Monad (liftM2)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (parseMaybe, sepBy)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Util

--------------------------------------------------------------------------------
-- Types

type Pos = (Int, Int)

data Path = Path Pos Pos
    deriving (Show)

data Material = Rock | Sand
    deriving (Show)

--------------------------------------------------------------------------------
-- Parsing and initialization

processLine :: Text -> [Path]
processLine t = zipWith Path xs (tail xs)
  where
    xs = fromJust $ parseMaybe @Pos (posP `sepBy` string " -> ") t
    posP = liftM2 (,) (decimal <* char ',') decimal

processInput :: Text -> M.Map Pos Material
processInput = foldr drawPath M.empty . concatMap processLine . T.lines

drawPath :: Path -> M.Map Pos Material -> M.Map Pos Material
drawPath path mp = M.union mp $ M.fromList $ map (,Rock) $ positions path
  where
    positions (Path (x1, y1) (x2, y2))
        | x1 == x2 = [(x1, z) | z <- rangeFrom y1 y2]
        | y1 == y2 = [(z, y1) | z <- rangeFrom x1 x2]
        | otherwise = error "drawPath: invalid path"
    rangeFrom p q = if p <= q then [p .. q] else [q .. p]

--------------------------------------------------------------------------------
-- Main logic

fallSand :: Pos -> M.Map Pos Material -> Maybe Pos
fallSand (x, y) mp
    | isAbyss (x, y) = Nothing
    | isFree down = fallSand down mp
    | isFree botleft = fallSand botleft mp
    | isFree botright = fallSand botright mp
    | otherwise = Just (x, y)
  where
    down = (x, y + 1)
    botleft = (x - 1, y + 1)
    botright = (x + 1, y + 1)
    isFree p = p `M.notMember` mp
    isAbyss p = snd p > maximum (snd . fst <$> M.toList mp)

--------------------------------------------------------------------------------
-- Solution

run :: Pos -> M.Map Pos Material -> Int
run = go 0
  where
    go i src mp = case fallSand src mp of
        Nothing -> i
        Just pos -> go (i + 1) src (M.insert pos Sand mp)

solveA :: Text -> Text
solveA = tshow . run (500, 0) . processInput

solveB :: Text -> Text
solveB = undefined

--------------------------------------------------------------------------------
