module Main where

import Options.Applicative
import Control.Monad       (join)
import System.IO           (openFile, IOMode(ReadMode), hGetContents, hClose)

data Args = Args {
    args'prefix  :: FilePath
  , args'data    :: FilePath
  , args'postfix :: FilePath
  , args'target  :: FilePath
} deriving (Show)

argsParser :: Parser Args
argsParser = Args <$> fileParam "prefix" "file that holds the prefix of the targetfile before the table"
                  <*> fileParam "data" "yaml file holding the primary data"
                  <*> fileParam "postfix" "file that holde the postfix of the targetfile after the table"
                  <*> fileParam "target" "target file to generate"
  where
    fileParam l h = strOption (long l <> metavar "FILE" <> help h)

main :: IO ()
main = do
    args <- execParser $ info argsParser fullDesc
    x    <- readFile $ args'prefix args

    putStrLn "LOL"
    putStrLn x

