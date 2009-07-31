module HSPass.Actions.Search ( searchCommand ) where

import Data.List ( isInfixOf )

import HSPass.Common.Index
import HSPass.Core

searchCommand path args config = withDatabase (passPrompt config) path search
  where search db = do
            if null results
               then putStrLn "No Results Found!" >> return Nothing
               else mapM showIndexed results     >> return Nothing
          where searchString = if null args then "" else head args
                filterDB = filter (isInfixOf searchString . name . snd)
                results = filterDB . zip [0..] $ db
