module JSONOutput where

import JSON
import Data.List (intercalate)


renderString :: String -> String 
renderString str = "\"" ++ renderStringHelper str 

renderStringHelper :: String -> String
renderStringHelper ""        = "\"" 
renderStringHelper ('"':xs)  = "\\\"" ++ renderStringHelper xs 
renderStringHelper (x:xs)    = x : renderStringHelper xs  

renderJSON :: JSON -> String
renderJSON (String s)      = renderString s
renderJSON (Boolean True)  = "true"
renderJSON (Boolean False) = "false"
renderJSON (Number n)      = show n
renderJSON Null            = "null"
renderJSON (Array a)       = case a of
                                [] -> "[]"
                                _  -> "[" ++ intercalate ", " (map renderJSON a) ++ "]"
renderJSON (Object o)      = "{" ++ intercalate ", " (map renderPair o) ++ "}"
                              where renderPair (key, value) = renderString key ++ ": " ++ renderJSON value
