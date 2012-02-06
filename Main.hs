module Main where

import Data.LHA
import Data.LHE as LHE

instance Show Event where
  show e = show $ parts e

instance Show Particle where
  show p = show $ partPDG p

main = do
  events <- LHE.parseEventFile "events.lhe"
  putStrLn $ show events
