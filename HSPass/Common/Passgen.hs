module HSPass.Common.Passgen ( generatePassword ) where

import System.Random

generatePassword :: Int -> String -> IO String
generatePassword pwlen chars = do
    stdGen <- newStdGen
    let rNums = take pwlen . randomRs (0, pred $ length chars) $ stdGen
    return $ map (chars!!) rNums
