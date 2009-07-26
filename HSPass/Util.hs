module HSPass.Util where

import HSPass.Passwords
import System.IO.Error (try)

showIndexed (i,p) = do putStrLn $ ""
                       putStrLn $ "Index #" ++ show i
                       putStrLn $ "Name:  " ++ name p
                       putStrLn $ "User:  " ++ user p
                       putStrLn $ "Pass:  " ++ pass p
                       putStrLn $ "Desc:  " ++ desc p

readIndex args = do
  if null args
     then putStrLn "No index provided!" >> return Nothing
     else do readResult <- try . readIO . head $ args
             case readResult of
                  Left  _ -> do putStrLn $ "Bad index '" ++ (head args) ++ "'"
                                return Nothing
                  Right i -> return . Just $ i
