module HSPass.Actions.Search ( searchCommand ) where

import HSPass.Common.Index
import HSPass.Common.Search
import HSPass.Core

searchCommand dbPath args config =
    withDatabase (passPrompt config) dbPath $ \db -> do
        let searchPhrase = if null args then "" else head args
        print . bestPassword 0.2 db $ searchPhrase
        let topScored = take 4 . rankPassword (zip [0..] db) $ searchPhrase
        if null topScored
           then putStrLn "No Results Found!" >> return Nothing
           else mapM showIndexed topScored   >> return Nothing
