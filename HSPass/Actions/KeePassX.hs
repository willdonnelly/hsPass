module HSPass.Actions.KeePassX
  ( keepassxExport
  , keepassxImport
  ) where

import HSPass.Core
import Data.Maybe
import Text.XML.Light
import HSPass.Common.Index
import System.Locale
import Data.Time

keepassxExport dbPath args config =
    withDatabase (passPrompt config) dbPath $ \db -> do
        let filePath = if null args
                          then "passwords.xml"
                          else head args
        time <- getCurrentTime
        let timeString = formatTime defaultTimeLocale "%FT%T" time
        let entries = concat $ map (passEntryToXML timeString) db
        let xml = xmlHeader ++ entries ++ xmlFooter
        writeFile filePath xml
        putStrLn xml
        return Nothing

passEntryToXML time (PassEntry n u p d s) = ppElement $ Element (QName "entry" Nothing Nothing) [] elems Nothing
  where elems = [ title, user, pass, url, comment, icon
                , create, access, modify, expire]
        title   = Elem $ elem "title"      n
        user    = Elem $ elem "username"   u
        pass    = Elem $ elem "password"   p
        url     = Elem $ elem "url"        s
        comment = Elem $ elem "comment"    d
        icon    = Elem $ elem "icon"       "0"
        create  = Elem $ elem "creation"   time
        access  = Elem $ elem "lastaccess" time
        modify  = Elem $ elem "lastmod"    time
        expire  = Elem $ elem "expire"     "Never"
        elem name text = Element (QName name Nothing Nothing) []
                                 [Text (CData CDataText text Nothing)] Nothing

xmlHeader = "<!DOCTYPE KEEPASSX_DATABASE>\n<database>\n <group>\n\
            \  <title>hsPass Exports</title>\n  <icon>0</icon>"
xmlFooter = "</group></database>"

keepassxImport dbPath args config = do
    putStrLn "This will destroy the current database."
    let filePath = if null args
                      then fail "No XML path provided."
                      else head args
    withDatabase (passPrompt config) dbPath $ \db -> do
        fileContents <- readFile filePath
        let newDB = processXML fileContents
        return . Just $ newDB

processXML string = map xmlToPassEntry . concat $ passes
  where tree = fromJust $ parseXMLDoc string
        groups = findChildren (QName "group" Nothing Nothing) tree
        passes = map (findChildren $ QName "entry" Nothing Nothing) groups

xmlToPassEntry xml = PassEntry n u p d s
  where n = getData . namedNode "title"    $ xml
        u = getData . namedNode "username" $ xml
        p = getData . namedNode "password" $ xml
        d = getData . namedNode "comment"  $ xml
        s = getData . namedNode "url"      $ xml
        namedNode name xml = findChild (QName name Nothing Nothing) xml
        getData = fromMaybe "" . fmap strContent
