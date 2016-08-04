module Main where

import Prelude
import Data.Either (either)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Node.Buffer (BUFFER)
import Node.FS.Async (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Data.String
import Data.Array
import Data.Foldable

main :: forall e. Eff (fs :: FS, console :: CONSOLE, err :: EXCEPTION, buffer :: BUFFER | e) Unit
main = do
  log "Hello sailor!"


day1Part1 :: forall e. Eff (fs :: FS, console :: CONSOLE, err :: EXCEPTION, buffer :: BUFFER | e) Unit
day1Part1 = do
  readTextFile UTF8 "./inputs/day1.txt" $ \x -> do
    either (log <<< show) (\x' ->
      log <<< show
      $ sum
      $ map (\c -> if c == '(' then 1 else -1)
      $ toCharArray
      $ trim x') x
