--------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module App (run) where

import Data.Text (Text)
import Data.Text.IO qualified as T
import Day01 qualified
import Day02 qualified
import Day03 qualified
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
solve _ _ = error "Invalid day and part"

--------------------------------------------------------------------------------
