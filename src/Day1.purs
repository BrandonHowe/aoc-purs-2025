module Day1
  (main)
  where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.List (List, foldl)
import Data.List as List
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

part1 :: List Int -> Int
part1 arr = foldl (+) 0 arr

part2 :: List Int -> Int
part2 _ = 67

main :: Effect Unit
main = do
    rawInput <- readTextFile UTF8 "./inputs/day1.txt"
    let inputLines = lines rawInput # Array.mapMaybe Int.fromString
    let inputList = List.fromFoldable inputLines
    _ <- log "Day 1:"
    logShow $ part1 inputList
    logShow $ part2 inputList