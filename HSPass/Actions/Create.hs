module HSPass.Actions.Create ( createCommand ) where

import HSPass.Util
import HSPass.Config

createCommand path args config@Config{editPass = edit} =
    withDatabase (passPrompt config) path $ \db -> do
        newPass <- edit $ defaultPass config
        return . Just $ db ++ [newPass]
