--------------------------------------------------------------------------------

module Day02 (solveA, solveB) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple.Extra (both)
import Util

--------------------------------------------------------------------------------

class (Bounded a, Enum a, Eq a) => CyclicEnum a where
    cpred :: a -> a
    cpred x = if x == minBound then maxBound else pred x

    csucc :: a -> a
    csucc x = if x == maxBound then minBound else succ x

data Move = Rock | Paper | Scissors
    deriving (Bounded, Enum, Eq)

instance CyclicEnum Move

moveFromCh :: Text -> Move
moveFromCh "A" = Rock
moveFromCh "B" = Paper
moveFromCh "C" = Scissors
moveFromCh "X" = Rock
moveFromCh "Y" = Paper
moveFromCh "Z" = Scissors
moveFromCh _ = error "Unexpected char"

data Outcome = Lose | Draw | Win
    deriving (Enum, Eq)

type Score = Int

shapeScore :: Move -> Score
shapeScore = (+ 1) . fromEnum

outcomeScore :: Outcome -> Score
outcomeScore = (* 3) . fromEnum

--------------------------------------------------------------------------------

processInput :: Text -> [(Text, Text)]
processInput = map processLine . T.lines
  where
    processLine = toTuple . T.words
    toTuple (x : y : _) = (x, y)
    toTuple _ = error "List has fewer than 2 elements"

--------------------------------------------------------------------------------

solveA :: Text -> Text
solveA = tshow . sum . map (score . both moveFromCh) . processInput

score :: (Move, Move) -> Score
score (m1, m2) =
    let outcome = play m1 m2
     in shapeScore m2 + outcomeScore outcome

-- | @play opponent you@
play :: Move -> Move -> Outcome
play m1 m2
    | m1 == m2 = Draw
    | csucc m1 == m2 = Win
    | otherwise = Lose

--------------------------------------------------------------------------------

solveB :: Text -> Text
solveB =
    tshow
        . sum
        . map (score' . bimap moveFromCh outcomeFromCh)
        . processInput

outcomeFromCh :: Text -> Outcome
outcomeFromCh "X" = Lose
outcomeFromCh "Y" = Draw
outcomeFromCh "Z" = Win
outcomeFromCh _ = error "Unexpected char"

score' :: (Move, Outcome) -> Score
score' (m1, o) = shapeScore (unplay m1 o) + outcomeScore o

-- | @unplay opponent outcome@
unplay :: Move -> Outcome -> Move
unplay m Draw = m
unplay m Win = csucc m
unplay m Lose = cpred m

--------------------------------------------------------------------------------
