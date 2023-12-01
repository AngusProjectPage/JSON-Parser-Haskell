module QueryInput where 

import ParserCombinators


-- These three parsers are responsible for looking for and removing the flags
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

-- Check for dash in input 
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

removeList :: [Query] -> Query 
removeList [query] = query 

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

--  Main parser controlling each run of parseCommandLine
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
