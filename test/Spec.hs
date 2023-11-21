import System.Exit
import Test.QuickCheck hiding (elements) 
import JSON 
import JSONOutput (renderJSON, renderString)
import Data.List (intercalate)
import QueryLanguage 
import JSONTransformer


-- Test with cabal test --test-show-details=streaming

instance Arbitrary JSON where
    arbitrary = sized genJson

genJson :: Int -> Gen JSON
genJson 0 = oneof
    [ String  <$> arbitrary 
    , Boolean <$> arbitrary 
    , pure Null
    , Number  <$> arbitrary 
    , Array   <$> genSingleItemJsonArray 
    , Object  <$> genSingleObject 
    ]

genJson n = oneof
    [ String  <$> arbitrary  
    , Boolean <$> arbitrary 
    , pure Null
    , Number  <$> arbitrary 
    , Array   <$> resize (n `div` 2) (listOf (genJson (n `div` 2)))
    , Object  <$> resize (n `div` 2) (listOf genObjectPair)
    ]

genObjectPair :: Gen (String, JSON)
genObjectPair = do
    key <- arbitrary
    value <- genJson 3 -- Change the size of the value in the object
    return (key, value)

genSingleItemJsonArray :: Gen [JSON]
genSingleItemJsonArray = do
    value <- genJson 0 
    return [value]

genSingleObject :: Gen [(String, JSON)]
genSingleObject = do 
    key <- arbitrary :: Gen String 
    value <- genJson 0 
    return [(key, value)]


instance Arbitrary Query where 
    arbitrary = genQuery 

genQuery :: Gen Query 
genQuery = oneof 
    [ Pipe        <$> genQuery <*> genQuery 
    , Field       <$> arbitrary
    , pure Elements 
    , Select      <$> genQuery 
    , ConstInt    <$> arbitrary 
    , ConstString <$> arbitrary
    , Equal       <$> genQuery <*> genQuery 
    ]





-- Testing JSON Transformer

-- Test string function 
propOnlyTransformedString :: String -> JSON -> Bool
propOnlyTransformedString s json = string s json == [String s] 

-- Test int function 
propOnlyTransformedInt :: Int -> JSON -> Bool 
propOnlyTransformedInt i json = int i json == [Number i]

-- Test elements function 
propElements :: JSON -> Bool 
propElements (Array a) = elements (Array a) == a 
propElements x         = elements x == []  

-- Test field function 
propField :: JSON -> String -> Bool 
propField (Object o) str = case getFieldFromObject str o of
                                    Just value -> length (field str (Object o)) > 0  
                                    Nothing    -> field str (Object o) == [] 
propField x          str = field str x == []

-- Test pipe Function 
propPipeInt :: JSON -> Int -> Int -> Bool 
propPipeInt j1 int1 int2 = pipe (int(int1)) (int(int2)) j1 == concat (map (int(int2)) (int(int1) j1)) 

propPipeString :: JSON -> String -> String -> Bool 
propPipeString j1 str1 str2 = pipe (string(str1)) (string(str2)) j1 == concat (map (string(str2)) (string(str1) j1)) 

--Test equal function 
propEqualString :: JSON -> String -> String -> Bool 
propEqualString js str1 str2 | elements js == [] && (t1 js) == (t2 js) = [Boolean True]  == equal t1 t2 js
                             | elements js == [] && (t1 js) /= (t2 js) = [Boolean False] == equal t1 t2 js
                             | leftList    == rightList                = [Boolean True]  == equal t1 t2 js
                             | otherwise                               = [Boolean False] == equal t1 t2 js 
                            where 
                                leftList  = [t1 a | a <- elements js]
                                rightList = [t2 b | b <- elements js]
                                t1 = string(str1) 
                                t2 = string(str2) 

-- Test select function 
propSelect :: JSON -> Bool
propSelect element | exists boolList = [element] == select transformer element
                   | otherwise       = []        == select transformer element
                        where
                            boolList = map (\x -> maybeBoolToBool (getBool x)) (transformer element)
                            transformer = elements   

-- Testing JSON Output

-- Test render JSON 
propRenderJSON :: JSON -> Bool
propRenderJSON (String s)      = renderJSON (String s)      == renderString s
propRenderJSON (Boolean True)  = renderJSON (Boolean True)  == "true"
propRenderJSON (Boolean False) = renderJSON (Boolean False) == "false"
propRenderJSON (Number n)      = renderJSON (Number n)      ==  show n 
propRenderJSON  Null           = renderJSON Null            == "null"
propRenderJSON (Array a)       = renderJSON (Array a)       == case a of
                                                                    [] -> "[]"
                                                                    _  -> "[" ++ intercalate ", " (map renderJSON a) ++ "]"
propRenderJSON (Object o)      = renderJSON (Object o)      == "{" ++ intercalate ", " (map renderPair o) ++ "}"
                                                                where
                                                                     renderPair (key, value) = renderString key ++ ": " ++ renderJSON value

   

-- Testing QueryLanguage 


-- Test execute function 


propExecute :: Query -> JSON -> Bool 
propExecute (Pipe q1 q2) js      = execute (Pipe q1 q2) js      == pipe (execute q1) (execute q2) js
propExecute (Field str)  js      = execute (Field str) js       == field str js 
propExecute (Elements)   js      = execute (Elements) js        == elements js
propExecute (Select q)   js      = execute (Select q) js        == select (execute q) js 
propExecute (ConstInt i) js      = execute (ConstInt i) js      == int i js
propExecute (ConstString str) js = execute (ConstString str) js == string str js
propExecute (Equal q1 q2) js     = execute (Equal q1 q2) js     == equal (execute q1) (execute q2) js 


main :: IO ()
main = do 
    quickCheck propOnlyTransformedString
    quickCheck propOnlyTransformedInt
    quickCheck propElements
    quickCheck propField
    quickCheck propPipeInt
    quickCheck propPipeString
    quickCheck propEqualString
    quickCheck propSelect 
    quickCheck propRenderJSON 
    quickCheck propExecute 


