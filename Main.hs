module Main where

import LHE as LHE

main = do
  putStr "Using LHE v" >> putStrLn LHE.version
  events <- LHE.parseRawEventFile "events.lhe"
  putStrLn $ show events
