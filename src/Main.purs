module Main where

import Prelude (Unit, show, (<<<), (+), (-), (==), ($), (<>))
import Data.Either (either)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Node.FS.Async (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Data.String (trim, toCharArray, split)
import Data.Foldable (foldl)
import Data.Array
import Debug.Trace

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

day6Part1 :: forall e. (String -> String) -> Eff (fs :: FS, console :: CONSOLE, err :: EXCEPTION | e) Unit
day6Part1 logg = do
  readTextFile UTF8 "./inputs/day6.txt"
    $ either
      (log <<< logg <<< show)
      (log
        <<< logg
        <<< show
        <<< length
        <<< filter (\b -> b)
        <<< concat
        <<< foldl processInstructions giantArray
        <<< mapWithIndex (\_ s -> trim s)
        <<< split "\n"
        <<< trim)

giantArray :: Array (Array Boolean)
giantArray = mapWithIndex (\_ _ -> mapWithIndex (\_ _ -> false) $ 1..1000) $ 1..1000

processInstructions :: Array (Array Boolean) -> String -> Array (Array Boolean)
processInstructions arr = processInstruction arr <<< split " "

processInstruction :: Array (Array Boolean) -> Array String -> Array (Array Boolean)
processInstruction arr [_, "off", origin, "through", endPoint] = arr
processInstruction arr [_, "on", origin, "through", endPoint] = arr
processInstruction arr ["toggle", origin, "through", endPoint] = arr
processInstruction arr _ = arr

switch :: (Bool -> Bool) -> Bool -> Arr Int -> Array (Array Boolean) -> Array (Array Boolean)
switch f default arr@[x, y] bigArr =
