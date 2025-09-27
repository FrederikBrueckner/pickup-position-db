{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text              (Text)
import           Data.Yaml              (FromJSON(..), withObject, decodeFileThrow, (.:), (.:?))

import           Options.Applicative    (execParser, strOption, long, metavar, help, info, fullDesc)
import qualified Options.Applicative as OA


data Args = Args {
    args'prefix  :: FilePath
  , args'data    :: FilePath
  , args'postfix :: FilePath
  , args'target  :: FilePath
} deriving (Show)

argsParser :: OA.Parser Args
argsParser = Args <$> fileParam "prefix" "file that holds the prefix of the targetfile before the table"
                  <*> fileParam "data" "yaml file holding the primary data"
                  <*> fileParam "postfix" "file that holde the postfix of the targetfile after the table"
                  <*> fileParam "target" "target file to generate"
  where
    fileParam l h = strOption (long l <> metavar "FILE" <> help h)

main :: IO ()
main = do
    args    <- execParser $ info argsParser fullDesc
    rawData <- decodeFileThrow $ args'data args
    let lineData = toLineData rawData

    putStrLn "LOL"
    putStrLn $ show rawData

toLineData :: [Yaml'Instrument] -> [LineData]
toLineData = const []

data LineData = LineData {
      ld'instrumentID :: Int
    , ld'brand        :: Text
    , lb'make         :: Text
    , lb'scale        :: Float
    , lb'description  :: Text
    , lb'value        :: Float
    , lb'reporter     :: Maybe Text
    , lb'comment      :: Maybe Text
    } deriving (Show)

-- representation of parsed data

data Yaml'Measurements = Yaml'Measurements {
      ym'description :: Text
    , ym'value       :: Float
    , ym'scale       :: Maybe Float
    , ym'comment     :: Maybe Text
    } deriving (Show)

instance FromJSON Yaml'Measurements where
  parseJSON = withObject "Measurement" $ \o ->
                Yaml'Measurements <$> o .:  "description"
                                  <*> o .:  "value"
                                  <*> o .:? "scale"
                                  <*> o .:? "comment"

data Yaml'Instrument = Yaml'Instrument {
      yi'brand        :: Text
    , yi'make         :: Text
    , yi'scale        :: Maybe Float
    , yi'reporter     :: Maybe Text
    , yi'comment      :: Maybe Text
    , yi'measurements :: [Yaml'Measurements]
    } deriving (Show)

instance FromJSON Yaml'Instrument where
  parseJSON = withObject "Instrument" $ \o ->
                Yaml'Instrument <$> o .:  "brand"
                                <*> o .:  "make"
                                <*> o .:? "scale"
                                <*> o .:? "reporter"
                                <*> o .:? "comment"
                                <*> o .:  "measurements"
