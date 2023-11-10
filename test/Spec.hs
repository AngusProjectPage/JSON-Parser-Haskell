import System.Exit
import Test.QuickCheck as Check
import JSON 
import JSONOutput (renderJSON)
import JSONTransformer


propReverse :: [Int] -> Bool 
propReverse xs = reverse (reverse xs) == xs 

-- JSON Output --  
--prop_renderJSON 


-- Generators are built from
-- choose :: Random a => (a, a) -> Gen a


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
    value <- genJson 3 -- Adjust the size of the value in the object
    return (key, value)

genSingleItemJsonList = do
    value <- genJson 0
    return [value]

genSingleObjectList = do 
    key <- arbitrary :: Gen String 
    value <- genJson 0 
    return [(key, value)]

-- Test string function 
prop_stringOnlyTransformedString :: String -> JSON -> Bool
prop_stringOnlyTransformedString s json = string s json == [String s] 

-- 


main :: IO ()
main = do quickCheck prop_stringOnlyTransformedString
