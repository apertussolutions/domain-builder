module Main where

import BinCdSchema
import SimpleKeyValue
import System.Exit
import System.Environment
import System.IO
import Text.Parsec(parse)
import qualified Data.Map as M

main = do
  (input, output) <- getPathArgs
  txt <- readFile input
  kv <- case parse parseFile input txt of
          Left e -> failure (show e)
          Right (File kv others)
                 -> return (M.toList kv ++ [ ("",o) | o <- others ])
  saveCdSchema output kv

getPathArgs =
  do xs <- getArgs
     case xs of
       [a,b] -> return (a,b)
       _     -> failure =<< usageString

usageString =
  do prog <- getProgName
     return $ prog ++ " INPUT_FILENAME OUTPUT_FILENAME"

failure str = hPutStrLn stderr str >> exitFailure

