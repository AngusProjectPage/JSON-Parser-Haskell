module JSONOutput where

import JSON
import Data.List (intercalate)

-- | Converts a 'String' into its string JSON representation by
-- placing quotes around it and escaping any quote characters that
-- appear in the string.
--
-- For example,
--
-- >  renderString "hello \"world\""
--
-- Returns
--
-- >  "\"hello \\\"world\\\"\""

renderString :: String -> String 
renderString str = "\"" ++ renderStringHelper str 

renderStringHelper :: String -> String
renderStringHelper ""        = "\"" 
renderStringHelper ('"':xs)  = "\\\"" ++ renderStringHelper xs 
renderStringHelper (x:xs)    = x : renderStringHelper xs  


-- HINT: one way of doing the escaping is the same way that we did the
-- htmlEscape function in the Week01 problems.

-- | Converts `JSON` into its string representation, as documented in
-- the JSON standard (https://www.json.org/json-en.html). The format
-- should be as is understood by the parsing functions in `JSONInput`.
--
-- For example,
--
--  > renderJSON (Object [("a",Number 1), ("b",Number 2)])
--
-- should give
--
--  > "{\"a\": 1, \"b\": 2}"
{-
renderJSON :: JSON -> String
renderJSON (String s)      = renderString s
renderJSON (Boolean True)  = "true"
renderJSON (Boolean False) = "false"
renderJSON (Number n)      =  show n 
renderJSON  Null           = "null"
renderJSON (Array a)       = case a of 
                              [] -> "[]"
                              _ -> "[" ++ concat(intersperse ", " (map renderJSON a)) ++ "]" 
renderJSON (Object o)      =  "{" ++ concat(intersperse ", " (concat(concat(b)))) ++ "}"
                              where a = (map (\(x,y) -> (renderString x, renderJSON y)) o) 
                                    b = [[[x ++ ": " ++ y]] | (x,y) <- a]
                                    -}
                             -- [(\"a\", "1"), (\"b\", "2"), (\"c\", "3")]
                             -- [[\"a\": 1], ]
                             -- [x,y | (x,y) <- a] where a = 
-- (Array [String "hello", Number 4, Null, Boolean False])

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

-- renderJSON (Object o)   
    
     --[("a",Number 1), ("b",Number 2)]

-- HINT: the `intersperse` function (imported above) is a good way of
-- putting something between every element of a list. It is the
-- "printing" counterpart of the 'sepBy' parser combinator.
--
-- You should use 'renderString' to implement rendering of 'String's
-- in the JSON.
