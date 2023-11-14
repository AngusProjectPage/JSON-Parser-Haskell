import System.Exit
import Test.QuickCheck 
import JSON 
import JSONOutput (renderJSON, renderString)
import Data.List (intersperse)
import JSONTransformer


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



main :: IO ()
main = do 
    quickCheck propOnlyTransformedString
    quickCheck propOnlyTransformedInt
    quickCheck propRenderJSON 

