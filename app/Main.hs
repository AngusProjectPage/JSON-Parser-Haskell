module Main (main) where

import System.Environment (getArgs)
import JSONInput
import JSONOutput
import ParserCombinators 
import QueryLanguage
import Result
import System.Exit 
import System.Directory 
import System.IO 
import Data.Char (isSpace, isLower, isUpper, isNumber,
                            digitToInt, isAlpha, isAlphaNum, isDigit)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Parser for command line options
-- newtype Parser a = MkParser (String -> Result (a, String)) -- So a stores parsed out stuff 
-- String stores the stuff to go into the next parser 
parseDashAndQuery :: Parser () 
parseDashAndQuery = do 
  whitespaces
  isChar '-' 
  isChar 'q'
  whitespaces

isAlphaNumNoSpace :: String -> Bool
isAlphaNumNoSpace = all (\c -> isAlphaNum c && not (isSpace c))

parseDashAndFiles :: Parser () 
parseDashAndFiles = do 
  whitespaces
  isChar '-' 
  isChar 'f'
  whitespaces

parseDashAndArguments :: Parser () 
parseDashAndArguments = do 
  whitespaces 
  isChar '-' 
  isChar 'a'
  whitespaces

noDash :: Parser Char
noDash =
  do c <- char
     case c of
       '-'  -> failParse ""
       c    -> return c

noDashList :: Parser Char 
noDashList = 
  do c <- char 
     case c of 
       '-' -> failParse ""
       ' ' -> failParse ""
       c -> return c 



parseCommandLine :: Parser ([Query],[String])
parseCommandLine = 
  do 
     parseDashAndQuery -- This outputs a parser of ((), Query+Files) 
     query <- zeroOrMore parseQuery
     parseDashAndFiles -- This outputs a parser of ((), Files) 
     files <- sepBy whitespace (zeroOrMore noDashList) -- This outputs a parser of ((), Files)
     return (query,files)  -- No input is consumed due to it being a monad


whitespaceAndBracket :: Parser ()
whitespaceAndBracket = do satisfies "No whitespace or bracket" (\c -> c == ' ' || c == '\n' || c == '\t' || c == '(' || c == ')')
                          return ()

-- | Parser for removing both whitespaces and brackets 
whitespaceAndBrackets :: Parser ()
whitespaceAndBrackets = do zeroOrMore whitespaceAndBracket
                           return ()


rmBracket :: Parser ()
rmBracket = do satisfies "Brackets" (\c -> c == '(' || c == ')')
               return ()

-- | Parser for removing both whitespaces and brackets 
rmBrackets :: Parser ()
rmBrackets = do zeroOrMore rmBracket
                return ()

--  Pipe Elements Select Equal Field 'Country' ConstString "S"
parseQuery :: Parser Query
parseQuery =    
  do 
     whitespaceAndBrackets
     stringLiteral "Elements"
     whitespaceAndBrackets
     return Elements
  `orElse`
  do 
     whitespaceAndBrackets
     stringLiteral "Field"
     whitespaceAndBrackets
     fd <- quotedString 
     whitespaceAndBrackets
     return (Field fd)
  `orElse`
  do 
     whitespaceAndBrackets
     stringLiteral "ConstInt"
     whitespaceAndBrackets
     ci <- number
     whitespaceAndBrackets
     return (ConstInt ci)
  `orElse`
  do 
    whitespaceAndBrackets
    stringLiteral "ConstString"
    whitespaceAndBrackets
    cs <- quotedString
    whitespaceAndBrackets
    return (ConstString cs)
  `orElse`
  do 
     whitespaceAndBrackets
     stringLiteral "Select"
     whitespaceAndBrackets
     item <- parseQuery 
     whitespaceAndBrackets
     return (Select item)
  `orElse`
  do 
     whitespaceAndBrackets
     stringLiteral "Equal"
     whitespaceAndBrackets
     item1 <- parseQuery
     rmBrackets
     item2 <- parseQuery
     whitespaceAndBrackets
     return (Equal item1 item2)
  `orElse`
  do 
     whitespaceAndBrackets
     stringLiteral "Pipe"
     whitespaceAndBrackets
     item1 <- parseQuery
     rmBrackets
     item2 <- parseQuery
     whitespaceAndBrackets
     return (Pipe item1 item2)
  `orElse`
  do 
     whitespaceAndBrackets
     stringLiteral "GreaterThan"
     whitespaceAndBrackets
     item1 <- parseQuery
     rmBrackets
     item2 <- parseQuery
     whitespaceAndBrackets
     return (GreaterThan item1 item2)
  `orElse`
  do 
     whitespaceAndBrackets
     stringLiteral "LessThan"
     whitespaceAndBrackets
     item1 <- parseQuery
     rmBrackets
     item2 <- parseQuery
     whitespaceAndBrackets
     return (LessThan item1 item2)
  `orElse`
  failParse "Couldn't parse Query"

--query :: Query
--query = Elements `Pipe` Select (Field 'Height' `LessThan` ConstInt 30)

--newQuery = Pipe (Elements) (Select (LessThan (Field \"Height\") (ConstInt 30)))

-- When entering query infix notation is not permitted in this program
main :: IO ()
main =
  do
    -- Retrieve command line input 
    [singleArgString] <- getArgs 

    -- Run the parser on the command line input 
    let result = runParser parseCommandLine singleArgString

    -- Check parsed result 
    case result of
      Ok (parsedOutput,leftover) ->
        -- Print parsed result to console
        do 
          putStrLn (show(fst parsedOutput))
         -- putStrLn $ "Query Parsed: " ++ show (fst parsedOutput) ++ ", Files Parsed: " ++ (head(snd parsedOutput))
        --inputJSON <- abortOnError (stringToJSON readFile((head(snd(parsedOutput)))))
        --let outputJSONs = execute (fst parsedOutput) inputJSON 
        --mapM_ (putStrLn . renderJSON) outputJSONs
      Error errorMessage ->
        putStrLn $ "Parsing error: " ++ errorMessage

    -- Check if all the files exist 
    -- rawText <- readFile filename 
    
    -- Parse the raw text of the input into a structured JSON
    -- representation.
    --
    -- FIXME: what if the user wants to
    --inputJSON <- abortOnError (stringToJSON rawText)

    -- Run the query on the parsed JSON to a list of JSON values
    --
    -- FIXME: What if the user wants a different query? the query
    -- should be taken as an input as well.
    --
    -- FIXME: the query langauge is quite inexpressive. What if the
    -- user wants all hills over 1000 metres in Scotland and Wales?
    -- or something else? What if they want to transform the input
    -- and not just filter it?
    --
    -- FIXME: The query might be incompatible with the input data. It
    -- might mention fields that the input does not have. Can these
    -- errors be reported back to the user nicely?

    -- FIXME: what if the user wants the JSON output to be nicely
    -- formatted? Or in colour? Or in another format, like HTML or
    -- CSV?
    --mapM_ (putStrLn . renderJSON) outputJSONs
