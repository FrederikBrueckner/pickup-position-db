{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where

import           Data.Aeson.Types       (typeMismatch)
import           Data.List              (intersperse)
import           Data.Maybe             (fromMaybe, catMaybes)
import qualified Data.Scientific     as DS
import           Data.Text              (Text, unpack)
import qualified Data.Text           as T
import           Data.Yaml              (FromJSON(..), withObject, decodeFileThrow, (.:), (.:?), Value(String, Number))
import qualified Data.Yaml           as Yaml

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
    Args{..} <- execParser $ info argsParser fullDesc
    rawData  <- decodeFileThrow args'data
    let lineData = toLineData rawData
        table = mkTable lineData

    prefix  <- readFile args'prefix
    postfix <- readFile args'postfix

    let output = prefix <> table <> postfix

    writeFile args'target output

mkTable :: [LineData] -> String
mkTable lineData = concat . intersperse "\n" $ headerLine : separatorLine : dataLines
  where
    headerLine :: String
    headerLine = formatCells $ map fst columns

    separatorLine :: String
    separatorLine = formatCells $ map (flip replicate '-' . length . fst) columns

    dataLines :: [String]
    dataLines = map singleDataLine lineData

    singleDataLine :: LineData -> String
    singleDataLine ld = formatCells $ map ( ($ ld) . snd ) columns

    formatCells :: [String] -> String
    formatCells = (<> " |") . ("| " <>) . concat . intersperse " | "

columns :: [(String, LineData -> String)]
columns = [ (" # "             , show . ld'instrumentID)
          , ("Brand"           , unpack . ld'brand)
          , ("Make"            , make)
          , ("Scale (inch)"    , show . ld'scale)
          , ("Pickup/Coil"     , unpack . ld'description)
          , ("Measurement (cm)", show . ld'value)
          , ("Normalized"      , show . ld'normalized)
          , ("Target 34 (cm)"  , targeted 34)
          , ("Target 32 (cm)"  , targeted 32)
          , ("Target 30 (cm)"  , targeted 30)
          , ("Reporter"        , maybe "" unpack . ld'reporter)
          , ("Comment"         , maybe "" unpack . ld'comment)
          ]
  where
    make LineData{..} = unpack $ T.concat [ld'make, maybe "" (\y -> " (" <> y <> ")") ld'year]

    targeted target = show . truncate' 1 . (*target) . ld'normalized

truncate' :: Int -> Float -> Float
truncate' n x = fromIntegral y / r
    where
      r = 10^n
      y = round (x * r) :: Int


toLineData :: [Yaml'Instrument] -> [LineData]
toLineData = concat . map (uncurry singleInstrument) . zip [1..]
  where
    singleInstrument :: Int -> Yaml'Instrument -> [LineData]
    singleInstrument index Yaml'Instrument{..} = map singleMeasurment yi'measurements
      where
        err e = error $ concat [show index, " / " , show yi'brand, " / ", show yi'make, " : ", e ]
        singleMeasurment :: Yaml'Measurment -> LineData
        singleMeasurment Yaml'Measurment{..} = LineData{..}
          where
            ld'instrumentID = index
            ld'brand        = yi'brand
            ld'make         = yi'make
            ld'scale        = case ym'scale of
                                Just  x -> x
                                Nothing -> fromMaybe (err "no scale defined") yi'scale
            ld'description  = ym'description
            ld'value        = ym'value
            ld'normalized   = truncate' 4 $ ld'value / ld'scale -- truncating to 4 gives us enough precision to compute the target position with mm precision
            ld'reporter     = yi'reporter
            ld'comment      = case catMaybes [yi'comment, ym'comment] of
                                [] -> Nothing
                                cs -> Just $ T.concat $ intersperse ". " cs
            ld'year         = yi'year

data LineData = LineData {
      ld'instrumentID :: Int
    , ld'brand        :: Text
    , ld'make         :: Text
    , ld'scale        :: Float
    , ld'description  :: Text
    , ld'value        :: Float
    , ld'normalized   :: Float
    , ld'reporter     :: Maybe Text
    , ld'comment      :: Maybe Text
    , ld'year         :: Maybe Text
    } deriving (Show)

-- representation of parsed data

data Yaml'Measurment = Yaml'Measurment {
      ym'description :: Text
    , ym'value       :: Float
    , ym'scale       :: Maybe Float
    , ym'comment     :: Maybe Text
    } deriving (Show)

instance FromJSON Yaml'Measurment where
  parseJSON = withObject "Measurement" $ \o ->
                Yaml'Measurment <$> o .:  "description"
                                <*> o .:  "value"
                                <*> o .:? "scale"
                                <*> o .:? "comment"

data Yaml'Instrument = Yaml'Instrument {
      yi'brand        :: Text
    , yi'make         :: Text
    , yi'scale        :: Maybe Float
    , yi'reporter     :: Maybe Text
    , yi'comment      :: Maybe Text
    , yi'measurements :: [Yaml'Measurment]
    , yi'year         :: Maybe Text
    } deriving (Show)



instance FromJSON Yaml'Instrument where
  parseJSON = withObject "Instrument" $ \o ->
                Yaml'Instrument <$> o .:  "brand"
                                <*> o .:  "make"
                                <*> o .:? "scale"
                                <*> o .:? "reporter"
                                <*> o .:? "comment"
                                <*> o .:  "measurements"
                                <*> (o .:? "year" >>= numOrStringToText)
    where
      numOrStringToText :: Maybe Value -> Yaml.Parser (Maybe Text)
      numOrStringToText Nothing  = pure Nothing
      numOrStringToText (Just x) = inner x
        where
          inner (Number scientific) = pure $ Just $ T.pack $ DS.formatScientific DS.Fixed (Just 0) scientific
          inner (String t)          = pure $ Just $ t
          inner e                   = typeMismatch "Number or String" e
