module Main where

import Prelude

import Data.Array (index)
import Data.Maybe (Maybe(..))
import Day1 as Day1
import Day2 as Day2
import Effect (Effect)
import Effect.Console (log)
import Node.Process (argv)

main :: Effect Unit
main = do
    args <- argv
    case index args 2 of
        Just day -> runDay day
        Nothing -> log "Usage: spago run <day> (e.g. spago run day12)"

runDay :: String -> Effect Unit
runDay = case _ of
    "day1" -> Day1.main
    "day2" -> Day2.main
    other -> log $ "Unknown day: " <> other