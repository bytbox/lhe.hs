module Main where

import Data.LHE as LHE

main = do
  putStr "Using LHE v" >> putStrLn LHE.version
  events <- LHE.parseRawEventFile "events.lhe"
  putStrLn $ show events
