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


parseQuery :: Parser String 
parseQuery = MkParser (\input -> 
                        case input of 
                          ('-':'q':query) -> Ok(span (\c -> c /= '-') query)
                          _ -> Error "Input must start with -q flag")

-- parseFile takes parseQuery as it's input 
parseCommandLine :: String -> Parser (String, [String]) 
parseCommandLine str = do 
  case runParser parseQuery str of 
    Ok (query, restOfString) -> do 
      let files = words(drop 2 restOfString)
      return (query, files) 
    Error msg -> failParse msg 
  

input :: String
input = "-q Elements `Pipe` Select (Field 'Country' `Equal` ConstString 'S') -f data/hills.json"


main :: IO ()
main =
  do -- Get the JSON filename to read from the command line arguments.
    --
    -- FIXME: What if
    -- we want to include additional command line options?

    -- Get files and arguments 
    commandLine <- getArgs 
    case runParser parseCommandLine input of 
      Ok (query, files) -> do 
        putStrLn query 
        putStrLn files 
      Error msg -> putStrLn msg
      
    -- Check if all the files exist 
    -- FIX ME
    --rawText <- readFile args 
    
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
    --let outputJSONs = execute query inputJSON
    {- 
    let outputJSONs = case query options of
                    Just q -> execute q inputJSON
                    Nothing -> execute query 
    -}
    -- Print the output, one per line.
    --
    -- FIXME: what if the user wants the JSON output to be nicely
    -- formatted? Or in colour? Or in another format, like HTML or
    -- CSV?
    --mapM_ (putStrLn . renderJSON) outputJSONs
