module Main where

import           Cats.App            (cats)
import           Data.Semigroup      ((<>))
import           Options.Applicative (Parser, ParserInfo, execParser, fullDesc,
                                      header, help, helper, info, long, metavar,
                                      progDesc, short, strOption)

main :: IO ()
main = run =<< execParser opts
  where
    run (CmdLineOptions dir) = cats dir

newtype CmdLineOptions = CmdLineOptions
  { directory :: FilePath
  }

optParser :: Parser CmdLineOptions
optParser =
  CmdLineOptions <$>
  strOption
    (long "dir" <> short 'd' <> metavar "DIRECTORY" <>
     help "Directory to analyse")

opts :: ParserInfo CmdLineOptions
opts =
  info
    (helper <*> optParser)
    (fullDesc <> progDesc "Analyse cohesion of a directory" <>
     header "cats - cohesion analysis for TypeScript")
