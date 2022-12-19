--------------------------------------------------------------------------------

module Day16 (solveA, solveB) where

import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Map ((!))
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, sepBy, some, (<|>))
import Text.Megaparsec.Char (letterChar, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Util

--------------------------------------------------------------------------------
-- Types

type Valve = Text
type Flow = Int
type Pressure = Int
type Minutes = Int

type Tunnel = M.Map Valve [Valve]
type FlowRates = M.Map Valve Flow

data Cave = Cave
    { tunnel :: Tunnel
    , flows :: FlowRates
    }

--------------------------------------------------------------------------------
-- Parsing

type Parser = Parsec Void Text
data ValveInfo = ValveInfo {valve :: Valve, flow :: Flow, adj :: [Valve]}
    deriving (Show)

processInput :: Text -> Cave
processInput t = Cave tunnel flows
  where
    tunnel = M.fromList $ map (\vi -> (vi.valve, vi.adj)) valveInfo
    flows = M.fromList $ map (\vi -> (vi.valve, vi.flow)) valveInfo
    valveInfo = map (fromJust . parseMaybe lineP) . T.lines $ t

    lineP :: Parser ValveInfo
    lineP = do
        valve <- string "Valve " *> valveP
        flow <- string " has flow rate=" *> decimal <* string "; "
        let valvesP = valveP `sepBy` string ", "
        vs <-
            (string "tunnels lead to valves " *> valvesP)
                <|> (string "tunnel leads to valve " *> valvesP)
        pure $ ValveInfo valve flow vs

    valveP :: Parser Valve
    valveP = T.pack <$> some letterChar

--------------------------------------------------------------------------------
-- Graph search

-- NOTE:
-- We'll do DFS on all the possible paths and pick the one with the maximum
-- reward. DFS ends once the "time limit" (i.e. cost) reaches 30.

data Path = Path
    { visited :: S.Set Valve
    , pressure :: Pressure
    , timeSpent :: Minutes
    }

timeLimit :: Minutes
timeLimit = 30

-- | @dfs cave pathSoFar currentValve@ returns the path satisfying the problem
-- from this point.
--
-- At some valve, we have two options:
--    1. Open it
--    2. Unopen it
-- then we move to the next valves.
dfs :: Cave -> Path -> Valve -> Path
dfs cave currPath currValve
    | currPath.timeSpent > timeLimit = currPath
    | currValve `S.member` currPath.visited = currPath
    | otherwise = bestPath $ concatMap exploreNext nextValves
  where
    exploreNext nv =
        [ dfs cave (moveToNext . markVisited $ currPath) nv
        , dfs cave (moveToNext . markOpened . markVisited $ currPath) nv
        ]
    markVisited p = p {visited = S.insert currValve p.visited}
    markOpened p =
        p
            { pressure =
                p.pressure
                    + openValvePressure p.timeSpent (cave.flows ! currValve)
            , timeSpent = p.timeSpent + 1
            }
    moveToNext p = p {timeSpent = p.timeSpent + 1}
    nextValves = cave.tunnel ! currValve

openValvePressure :: Minutes -> Flow -> Pressure
openValvePressure timeSpent flow = (timeLimit - timeSpent - 1) * flow

bestPath :: [Path] -> Path
bestPath = maximumBy (compare `on` (.pressure))

startDfs :: Cave -> Path
startDfs cave = dfs cave (Path S.empty 0 0) "AA"

--------------------------------------------------------------------------------
-- Solutions

solveA :: Text -> Text
solveA = tshow . (.pressure) . startDfs . processInput

solveB :: Text -> Text
solveB = undefined

--------------------------------------------------------------------------------
