module HSPass.Actions.Search ( searchCommand ) where

import Data.List
import Data.Char

import HSPass.Common.Index
import HSPass.Core

searchCommand path args config = withDatabase (passPrompt config) path search
  where search db = do
            if null topScored
               then putStrLn "No Results Found!" >> return Nothing
               else mapM showIndexed topScored   >> return Nothing
          where phrase = if null args then "" else head args
                score p = matchStrings phrase $ name p ++ desc p ++ site p
                rankDB = map (\(i, p) -> (score p, (i, p)))
                results = snd . unzip . sort . rankDB . zip [0..] $ db
                topScored = take 4 . reverse $ results

-- This will destroy words with <2 characters. Since those are more
-- likely to cause spurious matches, that's a good thing.
makeGrams string = zip string (tail string)

-- Calculate the number of matching trigrams
matchWords a b = length $ (makeGrams a) `intersect` (makeGrams b)

-- Match every combination of words in the two strings, and sum the
-- similarity scores. Higher == Better
matchStrings a b = sum . concat . map (\b -> map (matchWords b) wA) $ wB
  where wA = words . map (alphaNum . toLower) $ a
        wB = words . map (alphaNum . toLower) $ b
        alphaNum c = if isAlphaNum c then c else ' '
