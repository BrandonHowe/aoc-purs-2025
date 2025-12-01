module Day1
  ( main
  )
  where

import Prelude

import Data.Array (drop, filter, length, zip)
import Data.Array as Array
import Data.Foldable (sum)
import Data.Int as Int
import Data.List (List, foldl)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.String as Str
import Data.String.CodeUnits (charAt)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)

parseStr :: String -> Int
parseStr s =
  let
    dir = charAt 0 s # unsafePartial (\(Just c) -> c)
    numStr = Str.drop 1 s
    num = Int.fromString numStr # unsafePartial (\(Just c) -> c)
  in if dir == 'R' then num else -1 * num

part1 :: List Int -> Int
part1 arr = length (filter (_ == 0) (snd $ foldl go (Tuple 50 []) arr))
  where
    go (Tuple acc out) x =
      let acc' = acc + x in
      let acc2 = ((mod acc' 100) + 100) `mod` 100
      in Tuple acc2 (out <> [acc2])

part2 :: List Int -> Int
part2 arr = let sums = (snd $ foldl go (Tuple 50 []) arr) in
  let pairs = zip ([50] <> sums) (sums) in
  sum $ (\(Tuple a b) -> countMultiples a b) <$> pairs
  where
    go (Tuple acc out) x =
      let acc' = acc + x
      in Tuple acc' (out <> [acc'])
    countMultiples a b =
      let lo = min a b in
      let hi = max a b in
      max 0 ((if b > a then (hi - 1) / 100 else hi / 100) - (if b > a then (lo - 1) / 100 else lo / 100))

main :: Effect Unit
main = do
    rawInput <- readTextFile UTF8 "./inputs/day1.txt"
    let inputLines = lines rawInput <#> parseStr
    let inputList = List.fromFoldable inputLines
    _ <- log "Day 1:"
    logShow $ part1 inputList
    logShow $ part2 inputList