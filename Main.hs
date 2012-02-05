module Main where

import Data.LHE as LHE

main = do
  events <- LHE.parseRawEventFile "events.lhe"
  putStrLn $ show $ length events
