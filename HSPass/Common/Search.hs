module HSPass.Common.Search ( rankPassword, bestPassword ) where

import Data.List
import Data.Char

import HSPass.Core

-- | Generate a list of all substrings of a given length. Pads the string
--   with (n-1) spaces on each side so all characters get equal representation.
makeNGram :: Int -> Char -> String -> [String]
makeNGram l c str = take numGrams . transpose . map (`drop` buffered) $ [0..n]
  where n = pred l
        buffered = replicate n c ++ str ++ replicate n c
        numGrams = n + length str

-- | Generate the lists of N-Grams for each word, then return the ratio
--   of the number of identical N-Grams to the number of unique ones.
matchNGrams l c a b = matchedTokens / numTokenTypes
  where matchedTokens = fromIntegral . length $ aGrams `intersect` bGrams
        numTokenTypes = fromIntegral . length . nub $ aGrams ++ bGrams
        (aGrams, bGrams) = (makeNGram l c a, makeNGram l c b)

-- Compare the relative similarity of two strings
matchStrings l a b = map (\b -> map (matchNGrams l ' ' b) a) b

-- Take the best word score for every word in the query array
matchWords l text query = average wordScores
  where wordScores = map maximum $ matchStrings l text query
        average xs = sum xs / (fromIntegral $ length xs)

searchText text query = matchWords 2 (process text) (process query)
    where process = filter (not . (`elem`noise)) . preprocess
          preprocess = words . map (stripPunctuation . toLower)
          stripPunctuation c = if isAlphaNum c then c else ' '
          noise = ["a", "an", "the"]

rankPassword passes query = reverse . snd . unzip . sort $ zipScores
  where zipScores = [(scorePass query . snd $ pass, pass ) | pass <- passes]

scorePass q (PassEntry n u p d s) = searchText (unwords [n,u,d,s]) q

bestPassword t passes query = if scorePass query pass > t
                                 then Just pass
                                 else Nothing
  where (i, pass) = head $ rankPassword (zip [0..] passes) query
