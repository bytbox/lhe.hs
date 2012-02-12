{-|

This module provides a pure haskell implementation of a parser and writer for
the Les-Houches event file format, as described in hep-ph/0609017. (Note that
the writer doesn't actually exist yet.)

-}
module Data.LHE (
  parseFile,
  parse,
  parseEventFile,
  parseEvents,
) where

import qualified Data.ByteString.Char8 as S

import Text.XML.HaXml.ParseLazy (xmlParse)
import Text.XML.HaXml.Types (Document(..), Element(..), Content(..), QName(..))

import Data.LHA

data RawRun = RawRun [String] [[String]]
  deriving (Eq, Show)

data RawEvent = RawEvent [String] [[String]]
  deriving (Eq, Show)

parseFile :: String -> IO (Run, [Event])
parseFile fname = S.readFile fname >>= return . parse fname

parse :: String -> S.ByteString -> (Run, [Event])
parse fname dat =
  let doc = xmlParse fname $ S.unpack dat in
    (runFromDoc doc, eventsFromDoc doc)

parseEventFile :: String -> IO [Event]
parseEventFile fname = do
  S.readFile fname >>= return . parseEvents fname

parseEvents :: String -> S.ByteString -> [Event]
parseEvents fname dat =
  let re = parseRawEvents fname dat in
    map makeEvent re

runFromDoc = makeRun . rawRunFromDoc

eventsFromDoc d =
  let re = rawEventsFromDoc d in
    map makeEvent re

makeRun :: RawRun -> Run
makeRun (RawRun (bpdg1:bpdg2:be1:be2:pdfg1:pdfg2:pdfs1:pdfs2:idwt:nproc:[]) ps) = Run
  { runBeam = ( Beam (read bpdg1) (parseDouble be1) (read pdfg1) (read pdfs1)
              , Beam (read bpdg1) (parseDouble be1) (read pdfg1) (read pdfs1)
              )
  , idwt    = read idwt
  , nProc   = read nproc
  , procs   = map makeProc ps
  }

makeProc :: [String] -> Subprocess
makeProc (d1:d2:d3:i1:[]) = Subprocess
  (parseDouble d1)
  (parseDouble d2)
  (parseDouble d3)
  (read i1)

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
parseRawEventFile fname = S.readFile fname >>= return . parseRawEvents fname

parseRawEvents :: String -> S.ByteString -> [RawEvent]
parseRawEvents fname dat = rawEventsFromDoc $ xmlParse fname $ S.unpack dat

getElem (CElem (Elem _ _ c) _) = head $ filter isCString c
  where isCString (CString _ _ _) = True
        isCString _ = False

rawEventsFromDoc (Document _ _ (Elem eName _ eList) _) =
  map (getRawEvent . getElem) $ filter isEvent eList
  where
    isEvent (CElem (Elem (N "event") _ _) _) = True
    isEvent _ = False
    parseLine = words
    getRawEvent (CString _ eStr _) =
      let eLines = filter (\x -> length x > 2) $ lines eStr in
        RawEvent (parseLine $ head eLines) (map parseLine $ tail eLines)
    getRawEvent _ = RawEvent [] []

rawRunFromDoc (Document _ _ (Elem eName _ eList) _) =
  getRawRun $ getElem $ head $ filter isInit eList
  where
    isInit (CElem (Elem (N "init") _ _) _) = True
    isInit _ = False
    getRawRun (CString _ rStr _) =
      let rLines = filter (\x -> length x > 2) $ lines rStr in
        RawRun (words $ head rLines) (map words $ tail rLines)
    getRawRun _ = RawRun [] []

parseDouble :: String -> Double
parseDouble = read . reverse . dropWhile (=='.') . reverse

