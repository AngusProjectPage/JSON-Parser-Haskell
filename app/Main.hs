module Main (main) where

import System.Environment (getArgs)
import JSONInput
import QueryInput 
import JSONOutput
import ParserCombinators 
import QueryLanguage
import Result
import System.Exit 
import System.Directory 
import System.IO 
import Data.Char (isSpace, isLower, isUpper, isNumber,
                            digitToInt, isAlpha, isAlphaNum, isDigit)


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
        do 
          -- Read all input files
          filesRaw <- mapM readFile (snd parsedOutput)

          -- Convert file contents to JSON
          inputJSONs <- abortOnError (mapM stringToJSON filesRaw)

          -- Execute the query on each input JSON
          let outputJSONs = map (\(fileName, inputJSON) ->
                                  (fileName, execute (removeList (fst parsedOutput)) inputJSON))
                                (zip (snd parsedOutput) inputJSONs)

          -- Print the results with filenames
          mapM_ (\(fileName, outputJSON) -> do
                    putStrLn $ "\n\n" ++ "Results for file: " ++ fileName
                    mapM_ (putStrLn . renderJSON) outputJSON)
                outputJSONs
      Error errorMessage ->
        putStrLn $ "Parsing error: " ++ errorMessage
