module HSPass.Config where

import HSPass.Passwords
import HSPass.AutoType
import HSPass.Util
import HSPass.Edit

import System.IO                      ( hFlush, stdout )
import Data.List                      ( isInfixOf, isPrefixOf )
import System.Environment.XDG.BaseDir ( getUserDataFile )

type PluginCommand = [PassEntry] -> [String] -> Config -> IO (Maybe [PassEntry])

data Config = Config
    { errorMsg    :: Maybe String
    , editPass    :: PassEntry -> IO PassEntry
    , defaultPass :: PassEntry
    , passPath    :: IO String
    , plugins     :: [(String, PluginCommand)]
    , passPrompt  :: IO String
    }
defaultConfig = Config
    { errorMsg    = Nothing
    , editPass    = editPassword
    , defaultPass = PassEntry "" "" "" ""
    , passPath    = getUserDataFile "hsPass" "passwords"
    , plugins     = [ ("search", searchCommand)
                    , ("reveal", revealCommand)
                    , ("create", createCommand)
                    , ("modify", modifyCommand)
                    , ("delete", deleteCommand)
                    , ("autotype", autoType)
                    ]
    , passPrompt = do putStr "Password: "
                      hFlush stdout
                      getLine
    }
confError cfg msg = cfg { errorMsg = Just msg }

searchCommand db args config = do
    if null results
       then putStrLn "No Results Found!" >> return Nothing
       else mapM showIndexed results     >> return Nothing
  where searchString = if null args then "" else head args
        filterDB = filter (isInfixOf searchString . name . snd)
        results = filterDB . zip [0..] $ db

revealCommand db args config = do
    mapM showIndexed . zip [0..] $ db
    return Nothing

createCommand db args config@Config{editPass = edit} = do
    newPass <- edit $ defaultPass config
    return . Just $ db ++ [newPass]

modifyCommand db args config@Config{editPass = edit} = do
    index <- readIndex args
    case index of
         Nothing -> return Nothing
         Just ix -> do let (before, (current:after)) = splitAt ix db
                       newEntry <- edit current
                       return . Just $ before ++ [newEntry] ++ after

deleteCommand db args config = do
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
