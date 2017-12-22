{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List
import qualified Data.Text.Lazy     as TL
import qualified Data.Text.Lazy.IO  as TL
import           System.Environment

newtype Error =
  Error TL.Text

main :: IO ()
main = do
  [filename, columnName, replacement, outFilename] <- getArgs
  csvData <- TL.readFile filename
  let modifiedData =
        replaceColumn csvData (TL.pack columnName) (TL.pack replacement)
  case modifiedData of
    Right newData    -> TL.writeFile outFilename newData
    Left (Error err) -> TL.putStrLn err

replaceColumn :: TL.Text -> TL.Text -> TL.Text -> Either Error TL.Text
replaceColumn csv columnName replacement =
  case elemIndex columnName header of
    Just index ->
      Right $
      TL.unlines $
      map (TL.intercalate ",") $
      header : map (replaceRecord index replacement) records
    Nothing -> Left (Error "column not found in csv header")
  where
    csvLines = TL.lines csv
    cells = map (TL.splitOn ",") csvLines
    header = head cells
    records = tail cells

replaceRecord :: Int -> TL.Text -> [TL.Text] -> [TL.Text]
replaceRecord index replacement record = prefix ++ [replacement] ++ xs
  where
    (prefix, x:xs) = splitAt index record
