module Main where

import Prelude (Unit, show, (<<<), (+), (-), (==), ($), (<>))
import Data.Either (either)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Node.FS.Async (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Data.String (trim, toCharArray)
import Data.Foldable (foldl)

main :: forall e. Eff (fs :: FS, console :: CONSOLE, err :: EXCEPTION | e) Unit
main = do
  day1Part1 $ headerLog "Day 1 Part 1: "

headerLog :: String -> String -> String
headerLog header = (header <> _)

day1Part1 :: forall e. (String -> String) -> Eff (fs :: FS, console :: CONSOLE, err :: EXCEPTION | e) Unit
day1Part1 logg = do
  readTextFile UTF8 "./inputs/day1.txt"
    $ either
      (log <<< logg <<< show)
      (log <<< logg <<< show <<< foldl (\sum c -> if c == ')' then sum - 1 else sum + 1) 0 <<< toCharArray <<< trim)
