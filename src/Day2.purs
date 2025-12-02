module Day2
  ( main
  )
  where

import Prelude

import Data.Array (concatMap, drop, filter, foldl, head, length, range, reverse, tail, zip, (:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int64 (Int64)
import Data.Int64 as Int64
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.String (Pattern(..), split)
import Data.String as Str
import Data.String.CodeUnits (charAt, dropRight)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)

fromJust :: forall a. Partial => Maybe a -> a
fromJust (Just x) = x

intIsDouble :: Int64 -> Boolean
intIsDouble n =
  let s = dropRight 1 $ show n
      half = Str.length s / 2
  in Str.drop half s == Str.take half s

part1 :: Array (Tuple Int64 Int64) -> Int64
part1 arr = foldl go (Int64.fromInt 0) arr
  where
    go acc (Tuple lo hi) = loop lo acc
      where
        loop n acc'
          | n > hi = acc'
          | otherwise =
            loop (n + Int64.fromInt 1) (if intIsDouble n then acc' + n else acc')

part2 :: Array (Tuple Int64 Int64) -> Int64
part2 arr = foldl go (Int64.fromInt 0) arr
  where
    go acc (Tuple lo hi) = loop lo acc
      where
        intIsValid n = case regex "^(\\d+)\\1+$" noFlags of
          Left _ -> false
          Right r -> test r $ dropRight 1 $ show n
        loop n acc'
          | n > hi = acc'
          | otherwise =
            loop (n + Int64.fromInt 1) (if intIsValid n then acc' + n else acc')

main :: Effect Unit
main = do
    rawInput <- readTextFile UTF8 "./inputs/day2.txt"
    let inputStrs = split (Pattern ",") rawInput
    let inputPairs = split (Pattern "-") <$> inputStrs
    let inputPairsInts = (\x -> Tuple (unsafePartial $ fromJust $ Int64.fromString =<< head x) (unsafePartial $ fromJust $ Int64.fromString =<< head =<< tail x)) <$> inputPairs
    _ <- log "Day 2:"
    log $ dropRight 1 $ show $ part1 inputPairsInts
    log $ dropRight 1 $ show $ part2 inputPairsInts