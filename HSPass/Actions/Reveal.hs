module HSPass.Actions.Reveal ( revealCommand ) where

import HSPass.Common.Index
import HSPass.Core

revealCommand dbPath args config =
    withDatabase (passPrompt config) dbPath $ \db -> do
        mapM showIndexed . zip [0..] $ db
        return Nothing
