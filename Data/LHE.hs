{-|

This module provides a pure haskell implementation of a parser and writer for
the Les-Houches event file format. (Note that the writer doesn't actually exist
yet.)

-}
module Data.LHE (
  parseEventFile,
  parseEvents,

  parseRawEventFile,
  parseRawEvents,

  Event(..),
  RawEvent(..),
) where

import qualified Data.ByteString.Char8 as S

import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Types (Document(..), Element(..), Content(..), QName(..))

data Init = Init
  deriving (Eq, Show, Read)

data Event = Event [Double] [[Double]]
  deriving (Eq, Show, Read)

data RawEvent = RawEvent [Double] [[Double]]
  deriving (Eq, Show, Read)

parseEventFile :: String -> IO [Event]
parseEventFile fname = do
  S.readFile fname >>= return . parseEvents fname

parseEvents :: String -> S.ByteString -> [Event]
parseEvents fname dat =
  let re = parseRawEvents fname dat in
    []

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
    parseLine = map parseDouble . words
    getElem (CElem (Elem _ _ c) _) = head $ filter isCString c
    getRawEvent (CString _ eStr _) =
      let eLines = filter (\x -> length x > 2) $ lines eStr in
        RawEvent (parseLine $ head eLines) (map parseLine $ tail eLines)
    getRawEvent _ = RawEvent [] []

parseDouble :: String -> Double
parseDouble = read . reverse . dropWhile (=='.') . reverse

