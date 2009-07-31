module HSPass.Actions.Modify ( modifyCommand ) where

import HSPass.Common.Index
import HSPass.Core

modifyCommand dbPath args config@Config{editPass = edit} =
    withDatabase (passPrompt config) dbPath $ \db -> do
        index <- readIndex args
        case index of
             Nothing -> return Nothing
             Just ix -> do let (before, (current:after)) = splitAt ix db
                           newEntry <- edit current
                           return . Just $ before ++ [newEntry] ++ after
