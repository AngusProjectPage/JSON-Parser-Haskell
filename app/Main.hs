module Main (main) where

import System.Environment (getArgs)
import JSONInput
import JSONOutput
import ParserCombinators (runParser)
import QueryLanguage
import Result
import System.Exit 
import System.Directory 

query :: Query
query = Elements `Pipe` Select (Field "Country" `Equal` ConstString "S")

getQuery :: [String] -> [String] 
getQuery (x:xs) = if (x /= "q") then getQuery else       
  -- Look for flag, once true, take elements until another flag is found then finish 

getFiles :: [String] -> [String] 
getFiles = filter (\x -> strTake 2 == "-f") 

getStrings :: [String] -> [String] 
getStrings = filter (\x -> strTake 2 /= "-f") 

filesExist :: [String] -> Bool 
filesExist xs = all (==True) (map doesFileExist xs)

readFiles :: [String] -> [String] 
readFiles xs = map readFile xs 

-- Command is entered in format -q Query -f <Files> -a <Arguments>
-- Query comes after -q flag
-- Files come after -f flag 
-- Arguments come after -a flag   
main :: IO ()
main =
  do -- Get the JSON filename to read from the command line arguments.
     --
     -- FIXME: What if
     -- we want to include additional command line options?

     -- Get files and arguments 
     [args]   <- getArgs 
     [query]  <- getQuery args  
     [files]  <- getFiles args
     [args]   <- getArgs args

     -- Check if all the files exist 
     [filesExist] <- filesExist files 
     [rawText]    <- if filesExist then readFiles files else die "Error: File path does not exist, please enter a valid file path";


     -- Parse the raw text of the input into a structured JSON
     -- representation.
     --
     -- FIXME: what if the user wants to
     inputJSON <- abortOnError (stringToJSON rawText)
     query <- 

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
     let outputJSONs = execute query inputJSON

     -- Print the output, one per line.
     --
     -- FIXME: what if the user wants the JSON output to be nicely
     -- formatted? Or in colour? Or in another format, like HTML or
     -- CSV?
     mapM_ (putStrLn . renderJSON) outputJSONs
