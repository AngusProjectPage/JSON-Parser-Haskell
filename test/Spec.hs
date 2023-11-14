import System.Exit
import Test.QuickCheck hiding (elements) 
import JSON 
import JSONOutput (renderJSON, renderString)
import Data.List (intersperse)
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
    , Array   <$> genSingleItemJsonList
    , Object  <$> genSingleObjectList 
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

genSingleItemJsonList = do
    value <- genJson 0
    return [value]

genSingleObjectList = do 
    key <- arbitrary :: Gen String 
    value <- genJson 0 
    return [(key, value)]


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
propEqualInt :: JSON -> Int -> Int -> Bool 
propEqualInt j1 int1 int2 = 


-- Testing JSON Output

-- Test render JSON 
propRenderJSON :: JSON -> Bool
propRenderJSON (String s)      = renderJSON (String s)      == renderString s
propRenderJSON (Boolean True)  = renderJSON (Boolean True)  == "true"
propRenderJSON (Boolean False) = renderJSON (Boolean False) == "false"
propRenderJSON (Number n)      = renderJSON (Number n)      == show n 
propRenderJSON (Null)          = renderJSON Null            == "null"
propRenderJSON (Array a)       = renderJSON (Array a)       == "[" ++ concat(intersperse ", " (map renderJSON a)) ++ "]" 
propRenderJSON (Object o)      = renderJSON (Object o)      == "{" ++ concat(intersperse ", " (concat(concat(b)))) ++ "}"
                                where a = (map (\(x,y) -> (renderString x, renderJSON y)) o) 
                                      b = [[[x ++ ": " ++ y]] | (x,y) <- a] 


-- Addition test cases 




main :: IO ()
main = do 
    quickCheck propOnlyTransformedString
    quickCheck propOnlyTransformedInt
    quickCheck propRenderJSON 
    quickCheck propElements
    quickCheck propField
    quickCheck propPipeInt
    quickCheck propPipeString

