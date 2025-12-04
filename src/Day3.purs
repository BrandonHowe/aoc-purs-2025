module Day3
  ( main
  )
  where

import Data.Maybe
import Prelude

import Data.Array (drop, dropWhile, length, mapMaybe, take)
import Data.Foldable (maximum, sum)
import Data.Int64 (Int64)
import Data.Int64 as Int64
import Data.String (split, Pattern(..))
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)

pow :: Int64 -> Int64 -> Int64
pow b n = if n == Int64.fromInt 0 then Int64.fromInt 1 else b * (pow b (n - Int64.fromInt 1))

solve :: Int64 -> Array Int64 -> Int64
solve n xs =
    if n == Int64.fromInt 0 then case maximum xs of
  Nothing -> Int64.fromInt 0
  Just m -> m
  else 
  let usable = take (unsafePartial $ fromJust $ Int64.toInt (Int64.fromInt (length xs) - n)) xs
      largest = case maximum usable of
        Nothing -> Int64.fromInt 0
        Just m -> m
      rest = drop 1 (dropWhile (_ /= largest) xs)
  in largest * (pow (Int64.fromInt 10) n) + solve (n - Int64.fromInt 1) rest

main :: Effect Unit
main = do
    raw <- readTextFile UTF8 "inputs/day3.txt"
    let inputLines = lines raw
    let input = map (mapMaybe Int64.fromString <<< split (Pattern "")) inputLines
    logShow $ sum (map (solve $ Int64.fromInt 1) input)
    logShow $ sum (map (solve $ Int64.fromInt 11) input)