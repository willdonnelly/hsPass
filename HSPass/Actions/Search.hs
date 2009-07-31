module HSPass.Actions.Search ( searchCommand ) where

import HSPass.Common.Index
import HSPass.Common.Search
import HSPass.Core

searchCommand dbPath args config =
    withDatabase (passPrompt config) dbPath $ \db -> do
        let searchPhrase = if null args then "" else head args
        print . searchSingle 0.08 searchPhrase $ db
        let topScored = take 4 . searchDB searchPhrase $ db
        if null topScored
           then putStrLn "No Results Found!" >> return Nothing
           else mapM showIndexed topScored   >> return Nothing
