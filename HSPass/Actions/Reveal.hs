module HSPass.Actions.Reveal ( revealCommand ) where

import HSPass.Util
import HSPass.Config

revealCommand dbPath args config =
    withDatabase (passPrompt config) dbPath $ \db -> do
        mapM showIndexed . zip [0..] $ db
        return Nothing
