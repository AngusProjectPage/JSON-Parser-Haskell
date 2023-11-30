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

command :: Parser String
command =
  do cs <- zeroOrMore noDash
     return cs  -- Parse the input String as a query and return 


parseCommandLine :: Parser ([Query],[String])
parseCommandLine = 
  do 
     parseDashAndQuery -- This outputs a parser of ((), Query+Files) 
     query <- sepBy parseQuery (zeroOrMore noDashList)
     parseDashAndFiles -- This outputs a parser of ((), Files) 
     files <- sepBy whitespace (zeroOrMore noDashList) -- This outputs a parser of ((), Files)
     return (query,files)  -- No input is consumed due to it being a monad

identifier2 :: Parser String
identifier2 =
  oneOrMore (satisfies "alphanumeric character" isAlphaNum)

stringToQuery :: String -> Result Query
stringToQuery = completeParse parseQuery


parseQuery :: Parser Query
parseQuery =    
  do 
     whitespaces
     stringLiteral "Elements"
     whitespaces
     return Elements
  `orElse`
  do 
     whitespaces 
     stringLiteral "Field"
     whitespace
     fd <- identifier2
     whitespaces 
     return (Field fd)
  `orElse`
  do 
     whitespaces 
     stringLiteral "ConstInt"
     whitespace
     ci <- number
     whitespaces
     return (ConstInt ci)
  `orElse`
  do 
    whitespaces
    stringLiteral "ConstString"
    whitespace
    cs <- identifier2
    whitespaces
    return (ConstString cs)
  `orElse`
  do 
     whitespaces
     stringLiteral "Select"
     whitespace
     item <- parseQuery 
     whitespaces
     return (Select item)
  `orElse`
  do 
     whitespaces
     stringLiteral "Equal"
     whitespace
     item1 <- parseQuery
     whitespace
     item2 <- parseQuery
     whitespaces
     return (Equal item1 item2)
  `orElse`
  do 
     whitespaces
     stringLiteral "Pipe"
     whitespace
     item1 <- parseQuery
     whitespace
     item2 <- parseQuery
     whitespaces
     return (Pipe item1 item2)
  `orElse`
  do 
     whitespaces
     stringLiteral "GreaterThan"
     whitespace
     item1 <- parseQuery
     whitespace
     item2 <- parseQuery
     whitespaces
     return (GreaterThan item1 item2)
  `orElse`
  do 
     whitespaces
     stringLiteral "LessThan"
     whitespace
     item1 <- parseQuery
     whitespace
     item2 <- parseQuery
     whitespaces
     return (LessThan item1 item2)
  `orElse`
  failParse "Couldn't parse Query"

--query :: Query
--query = Elements `Pipe` Select (Field 'Height' `LessThan` ConstInt 30)


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
          mapM_ putStrLn (show(fst parsedOutput))
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
