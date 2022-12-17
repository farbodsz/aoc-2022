--------------------------------------------------------------------------------

module Day10 (solveA, solveB) where

import Data.List.Split (chunksOf)
import Data.Text (Text)
import Data.Text qualified as T
import Util

--------------------------------------------------------------------------------

type Value = Int
data Instr = Noop | Addx Value
    deriving (Show)

processInput :: Text -> [Instr]
processInput = map parseLine . T.lines
  where
    parseLine t = case T.words t of
        ["noop"] -> Noop
        ["addx", i] -> Addx (tread i)
        _ -> error "parseLine"

run :: [Instr] -> [Value]
run = scanl (+) 1 . concatMap go
  where
    go Noop = [0]
    go (Addx v) = [0, v]

solveA :: Text -> Text
solveA =
    tshow
        . sum
        . map (uncurry (*) . head)
        . chunksOf 40
        . drop 19
        . zip [1 ..]
        . run
        . processInput

--------------------------------------------------------------------------------

drawRow :: [(Int, Value)] -> Text
drawRow = T.concat . map (uncurry drawPx)
  where
    drawPx c val =
        let spritePos = [val - 1 .. val + 1]
            crtRow = c - 1
         in if crtRow `elem` spritePos then "#" else "."

solveB :: Text -> Text
solveB =
    T.unlines
        . map (drawRow . zip [1 ..])
        . chunksOf 40
        . init
        . run
        . processInput

--------------------------------------------------------------------------------
