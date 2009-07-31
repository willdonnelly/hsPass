module HSPass.Actions.Create ( createCommand ) where

import HSPass.Core

createCommand path args config@Config{editPass = edit} =
    withDatabase (passPrompt config) path $ \db -> do
        password <- genPassword config
        newPass <- edit $ (defaultPass config) {pass = password}
        return . Just $ db ++ [newPass]
