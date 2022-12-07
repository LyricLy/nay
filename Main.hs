module Main where

import Interpreter
import Parser
import Text.Megaparsec
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Map (Map, fromList)
import System.Environment

std :: Map Text Value
std = fromList []

runNay :: Text -> IO ()
runNay prog = case parse program "code" prog of
  Left bundle -> putStrLn (errorBundlePretty bundle)
  Right e -> eval std e >>= print


main = do
  [f] <- getArgs
  s <- TIO.readFile f
  runNay s
