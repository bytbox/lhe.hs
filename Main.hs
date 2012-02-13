module Main where

import Data.LHA
import Data.LHE as LHE

instance Show Event where
  show e = show $ parts e

instance Show Particle where
  show p = show $ partPDG p

instance Show Run where
  show = show . runBeam

instance Show Beam where
  show = show . beamPDG

main = do
  (run, events) <- LHE.parseFile "events.lhe"
  putStrLn $ show run
  putStrLn $ show $ length events
