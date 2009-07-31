module HSPass.Actions.Reveal ( revealCommand ) where

import HSPass.Common.Index
import HSPass.Common.Database
import HSPass.Core

revealCommand dbPath args config =
    withDatabase (passPrompt config) dbPath $ \db -> do
        mapM showIndexed . zip [0..] $ db
        return Nothing
