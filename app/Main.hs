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

-- Parser for command line options
-- newtype Parser a = MkParser (String -> Result (a, String))

{-
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

{-
parseDashAndArguments :: Parser () 
parseDashAndArguments = do 
  whitespaces
  isChar '-' 
  isChar 'a'
  whitespaces
-}
-}

     -- Places all the spaces into (text, ()) second part of tuple
{-
noDash :: Parser Char
noDash =
  do c <- char
     case c of
       '-'  -> failParse ""
       c    -> return c

command :: Parser String
command =
  do cs <- zeroOrMore noDash
     return cs

-- Separated by the flags
parseCommandLine :: Parser (String,[String])
parseCommandLine = 
  do 
     parseDashAndQuery 
     query <- command 
     parseDashAndFiles
     files <- command sepBy whitespace 
     return (query, files)  

-}

query :: Query
query = Elements `Pipe` Select (Field "Height" `LessThan` ConstInt 30)



main :: IO ()
main =
  do -- Get the JSON filename to read from the command line arguments.
    --
    -- FIXME: What if
    -- we want to include additional command line options?
    {-
    (query, files, remainder) <- runParser parseCommandLine input
    putStrLn query  
    -}
    [filename] <- getArgs 
      
    -- Check if all the files exist 
    -- FIX ME
    rawText <- readFile filename 
    
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
    inputJSON <- abortOnError (stringToJSON rawText)
    -- Print the output, one per line.
    let outputJSONs = execute query inputJSON 

    mapM_ (putStrLn . renderJSON) outputJSONs
    -- FIXME: what if the user wants the JSON output to be nicely
    -- formatted? Or in colour? Or in another format, like HTML or
    -- CSV?
    --mapM_ (putStrLn . renderJSON) outputJSONs
