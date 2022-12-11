--------------------------------------------------------------------------------

module Day09 (solveA, solveB) where

import Control.Monad (replicateM_)
import Control.Monad.State (MonadState (get, put), State, execState, modify)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Util

--------------------------------------------------------------------------------

data Direction = U | R | D | L
    deriving (Read, Show)

opposite :: Direction -> Direction
opposite U = D
opposite D = U
opposite L = R
opposite R = L

data Move = Move {direction :: Direction, amount :: Int}
    deriving (Read, Show)

processInput :: Text -> [Move]
processInput = map processLine . T.lines
  where
    processLine t = case T.words t of
        [dir, amount] -> Move (tread dir) (tread amount)
        _ -> error "processInput: cannot read"

--------------------------------------------------------------------------------

type Pos = (Int, Int)

distance :: Pos -> Pos -> Int
distance (x1, y1) (x2, y2) = max (abs $ x1 - x2) (abs $ y1 - y2)

newPos :: Move -> Pos -> Pos
newPos (Move dir n) (x, y) = case dir of
    U -> (x, y + n)
    D -> (x, y - n)
    L -> (x - n, y)
    R -> (x + n, y)

--------------------------------------------------------------------------------

data St = St {headPos :: Pos, tailPos :: Pos, visited :: S.Set Pos}

initState :: St
initState = St {headPos = pos0, tailPos = pos0, visited = S.fromList [pos0]}
  where
    pos0 = (0, 0)

move :: Move -> State St ()
move m = replicateM_ m.amount $ moveOne m.direction

moveOne :: Direction -> State St ()
moveOne dir = do
    modify $ \state -> state {headPos = newPos (Move dir 1) state.headPos}
    state <- get
    if distance state.headPos state.tailPos <= 1
        then pure ()
        else do
            let tailDir = opposite dir
            let newTailPos = newPos (Move tailDir 1) state.headPos
            put $
                state
                    { tailPos = newTailPos
                    , visited = S.insert newTailPos state.visited
                    }

--------------------------------------------------------------------------------

solveA :: Text -> Text
solveA = tshow . S.size . (.visited) . runProblem . processInput

runProblem :: [Move] -> St
runProblem ms = execState (mapM_ move ms) initState

solveB :: Text -> Text
solveB = undefined

--------------------------------------------------------------------------------
