--------------------------------------------------------------------------------

module Day03 (solveA, solveB) where

import Data.Char
import Data.List (intersect)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple.Extra (both)
import Util

--------------------------------------------------------------------------------

type Item = Char

data Rucksack = Rucksack [Item] [Item]
    deriving (Show)

ritems :: Rucksack -> [Item]
ritems (Rucksack xs ys) = xs <> ys

priority :: Item -> Int
priority ch = ord ch - diff
  where
    diff
        | isLower ch = 96
        | isUpper ch = 38
        | otherwise = error "Bad character"

--------------------------------------------------------------------------------

processInput :: Text -> [Rucksack]
processInput = map processLine . T.lines

processLine :: Text -> Rucksack
processLine t =
    uncurry Rucksack $
        both toItems $
            T.splitAt (T.length t `div` 2) t
  where
    toItems = T.unpack

--------------------------------------------------------------------------------

solveA :: Text -> Text
solveA = tshow . sum . map solve . processInput

solve :: Rucksack -> Int
solve (Rucksack xs ys) = priority $ head $ intersect xs ys

--------------------------------------------------------------------------------

solveB :: Text -> Text
solveB = tshow . sum . map solve' . groupIntoNs 3 . processInput

solve' :: [Rucksack] -> Int
solve' = priority . head . foldr1 intersect . map ritems

groupIntoNs :: Int -> [a] -> [[a]]
groupIntoNs n zs =
    let (xs, ys) = splitAt n zs
     in case ys of
            [] -> [xs]
            ys' -> xs : groupIntoNs n ys'

--------------------------------------------------------------------------------
