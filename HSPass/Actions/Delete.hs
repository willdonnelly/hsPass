module HSPass.Actions.Delete ( deleteCommand ) where

import System.IO ( hFlush, stdout )
import Data.List ( isPrefixOf )

import HSPass.Common.Index
import HSPass.Core

deleteCommand dbPath args config =
    withDatabase (passPrompt config) dbPath $ \db -> do
        index <- readIndex args
        case index of
             Nothing -> return Nothing
             Just ix -> do really <- confirmDelete ix (db !! ix)
                           if really
                              then return . Just $ deleted ix db
                              else return Nothing
  where confirmDelete idx pass = do
            putStrLn $ "You have chosen to delete:"
            showIndexed $ (idx, pass)
            putStr $ "Are you sure [y/N]: "
            hFlush stdout
            choice <- getLine
            if "Y" `isPrefixOf` choice || "y" `isPrefixOf` choice
               then putStrLn "Okay, deleting!" >> return True
               else putStrLn "Delete aborted." >> return False
        deleted v db = let (before, (_:after)) = splitAt v db in before ++ after
