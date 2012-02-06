{-|

This module provides a pure haskell implementation of a parser and writer for
the Les-Houches event file format, as described in hep-ph/0609017. (Note that
the writer doesn't actually exist yet.)

-}
module Data.LHE (
  parseEventFile,
  parseEvents,
) where

import qualified Data.ByteString.Char8 as S

import Text.XML.HaXml.ParseLazy (xmlParse)
import Text.XML.HaXml.Types (Document(..), Element(..), Content(..), QName(..))

import Data.LHA

data RawEvent = RawEvent [String] [[String]]
  deriving (Eq, Show)

parseEventFile :: String -> IO [Event]
parseEventFile fname = do
  S.readFile fname >>= return . parseEvents fname

parseEvents :: String -> S.ByteString -> [Event]
parseEvents fname dat =
  let re = parseRawEvents fname dat in
    map makeEvent re

makeEvent :: RawEvent -> Event
makeEvent (RawEvent (n:idpr:xwgt:scal:aqed:aqcd:[]) rps) = Event
  { nPart     = read n
  , evProcId  = read idpr
  , evWeight  = parseDouble xwgt
  , scale     = parseDouble scal
  , aQED      = parseDouble aqed
  , aQCD      = parseDouble aqcd
  , parts     = map makeParticle rps
  }

makeParticle :: [String] -> Particle
makeParticle (pdg:stat:m1:m2:c1:c2:px:py:pz:e:m:lt:spin:[]) = Particle
  { partPDG   = read pdg
  , status    = statusFromInt $ read stat
  , mothers   = PBoth (read m1, read m2)
  , iColor    = (read c1, read c2)
  , partPx    = parseDouble px
  , partPy    = parseDouble py
  , partPz    = parseDouble pz
  , partE     = parseDouble e
  , partM     = parseDouble m
  , lifetime  = parseDouble lt
  , spin      = parseDouble spin
  }

parseRawEventFile :: String -> IO [RawEvent]
parseRawEventFile fname = do
  S.readFile fname >>= return . parseRawEvents fname

parseRawEvents :: String -> S.ByteString -> [RawEvent]
parseRawEvents fname dat =
  let Document _ _ (Elem eName _ eList) _ = xmlParse fname (S.unpack dat) in
    map (getRawEvent . getElem) $ filter isEvent eList
  where
    isEvent (CElem (Elem (N "event") _ _) _) = True
    isEvent _ = False
    isCString (CString _ _ _) = True
    isCString _ = False
    parseLine = words
    getElem (CElem (Elem _ _ c) _) = head $ filter isCString c
    getRawEvent (CString _ eStr _) =
      let eLines = filter (\x -> length x > 2) $ lines eStr in
        RawEvent (parseLine $ head eLines) (map parseLine $ tail eLines)
    getRawEvent _ = RawEvent [] []

parseDouble :: String -> Double
parseDouble = read . reverse . dropWhile (=='.') . reverse

