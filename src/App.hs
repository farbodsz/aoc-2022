--------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module App (run) where

import Data.Text (Text)
import Data.Text.IO qualified as T
import Day01 qualified
import Day02 qualified
import Day03 qualified
import Day04 qualified
import Day05 qualified
import Day06 qualified
import Day07 qualified
import Day08 qualified
import Day09 qualified
import Day10 qualified
import Day11 qualified
import Day12 qualified
import Day13 qualified
import Day14 qualified
import Day15 qualified
import Day16 qualified
import Options.Applicative

--------------------------------------------------------------------------------

run :: IO ()
run =
    runCli =<< execParser opts
  where
    opts = info (aocCliP <**> helper) fullDesc

--------------------------------------------------------------------------------

type Day = Int
type Part = Int

data AocOpts = AocOpts
    { day :: Day
    , part :: Part
    , inputFile :: FilePath
    , outputFile :: FilePath
    }

aocCliP :: Parser AocOpts
aocCliP =
    AocOpts
        <$> argument auto (metavar "DAY")
        <*> argument auto (metavar "PART")
        <*> strArgument (metavar "INPUT")
        <*> strArgument (metavar "OUTPUT")

--------------------------------------------------------------------------------

runCli :: AocOpts -> IO ()
runCli opts = do
    inputContents <- T.readFile opts.inputFile
    T.writeFile opts.outputFile $ solve opts.day opts.part inputContents

solve :: Day -> Part -> (Text -> Text)
solve 1 1 = Day01.solveA
solve 1 2 = Day01.solveB
solve 2 1 = Day02.solveA
solve 2 2 = Day02.solveB
solve 3 1 = Day03.solveA
solve 3 2 = Day03.solveB
solve 4 1 = Day04.solveA
solve 4 2 = Day04.solveB
solve 5 1 = Day05.solveA
solve 5 2 = Day05.solveB
solve 6 1 = Day06.solveA
solve 6 2 = Day06.solveB
solve 7 1 = Day07.solveA
solve 7 2 = Day07.solveB
solve 8 1 = Day08.solveA
solve 8 2 = Day08.solveB
solve 9 1 = Day09.solveA
solve 9 2 = Day09.solveB
solve 10 1 = Day10.solveA
solve 10 2 = Day10.solveB
solve 11 1 = Day11.solveA
solve 11 2 = Day11.solveB
solve 12 1 = Day12.solveA
solve 12 2 = Day12.solveB
solve 13 1 = Day13.solveA
solve 13 2 = Day13.solveB
solve 14 1 = Day14.solveA
solve 14 2 = Day14.solveB
solve 15 1 = Day15.solveA
solve 15 2 = Day15.solveB
solve 16 1 = Day16.solveA
solve 16 2 = Day16.solveB
solve _ _ = error "Invalid day and part"

--------------------------------------------------------------------------------
