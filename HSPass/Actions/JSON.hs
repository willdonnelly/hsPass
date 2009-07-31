module HSPass.Actions.JSON
  ( exportJSON
  , importJSON
  ) where

import Text.JSON
import Text.JSON.Types
import Text.JSON.String
import Data.Maybe
import HSPass.Core

exportJSON dbPath args config =
    withDatabase (passPrompt config) dbPath $ \db -> do
        let filePath = if null args then "passwords.json"
                                    else head args
        let json = ($"") . showJSValue . JSArray . map passToJSON $ db
        writeFile filePath json
        return Nothing

passToJSON (PassEntry n u p d s) = JSObject . toJSObject $
  [ ("name", JSString . toJSString $ n)
  , ("user", JSString . toJSString $ u)
  , ("pass", JSString . toJSString $ p)
  , ("desc", JSString . toJSString $ d)
  , ("site", JSString . toJSString $ s)
  ]

importJSON dbPath args config =
    withDatabase (passPrompt config) dbPath $ \db -> do
        let filePath = if null args then "passwords.json"
                                    else head args
        json <- readFile filePath
        case runGetJSON readJSArray json of
             Left str -> return Nothing
             Right (JSArray ar) -> return . Just . map jsonToPass $ ar

jsonToPass (JSObject json) = PassEntry n u p d s
  where n = getValue "name"
        u = getValue "user"
        p = getValue "pass"
        d = getValue "desc"
        s = getValue "site"
        getValue = fromMaybe "" . fmap shellString . get_field json
        shellString (JSString a) = fromJSString a
