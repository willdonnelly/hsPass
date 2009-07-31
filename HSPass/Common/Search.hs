module HSPass.Common.Search ( searchDB, searchSingle ) where

import Data.List
import Data.Char
import HSPass.Core

-- Return a single result if the score is significantly better
-- then the next closest one
searchSingle threshold phrase db
    | diff >= threshold  =  Just a
    | otherwise         =  Nothing
  where ((x,a):(y,b):[]) = take 2 . searchDB phrase $ db
        diff     = (scorePass phrase a) - (scorePass phrase b)

-- Search the database for the given phrase. Returns a list
-- sorted by match quality.
searchDB phrase db = reverse results
  where rankDB = map (\(i, p) -> (scorePass phrase p, (i, p)))
        results = snd . unzip . sort . rankDB . zip [0..] $ db

scorePass phrase p = matchStrings phrase $ name p ++ ' ':desc p ++ ' ':site p

-- This will destroy words with <3 characters. Since those are more
-- likely to cause spurious matches, that's a good thing.
makeGrams string = zip3 string (tail string) (tail $ tail string)

-- Calculate the number of matching trigrams
matchWords a b = length $ (makeGrams a) `intersect` (makeGrams b)

-- Match every combination of words in the two strings, and sum the
-- similarity scores. Higher == Better
matchStrings a b = (fromIntegral rawScore) / (fromIntegral $ length a + length b)
  where rawScore = sum . concat . map (\b -> map (matchWords b) wA) $ wB
        wA = words . map (alphaNum . toLower) $ a
        wB = words . map (alphaNum . toLower) $ b
        alphaNum c = if isAlphaNum c then c else ' '
