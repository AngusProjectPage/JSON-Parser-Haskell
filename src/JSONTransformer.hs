module JSONTransformer (Transformer, field, select, pipe, string, int, equal, elements, exists, maybeBoolToBool, greaterThan, lessThan) where 
import JSON


type Transformer = JSON -> [JSON]

string :: String -> Transformer
string s _  = [String s]


int :: Int -> Transformer
int n _  = [Number n]


elements :: Transformer
elements o = case getElements o of 
                Nothing -> []
                Just element -> element 


field :: String -> Transformer
field s o = 
    case getField s o of
        Nothing -> []
        Just value -> [value]


pipe :: Transformer -> Transformer -> Transformer
pipe f g = concat . map g . f 


toBool :: Bool -> JSON
toBool  True  = Boolean True 
toBool  False = Boolean False


equal :: Transformer -> Transformer -> Transformer
equal t1 t2 xs = finalList 
                 where finalList = [toBool(x==y) | x <- leftList, y <- rightList]
                       leftList  = t1 xs
                       rightList = t2 xs


newtype Any = MkAny Bool 
    deriving Show 

instance Semigroup Any where 
    MkAny b1 <> MkAny b2 = MkAny (b1 || b2)

instance Monoid Any where 
    mempty = MkAny False 

unAny :: Any -> Bool 
unAny (MkAny b) = b

maybeBoolToBool :: Maybe Bool -> Bool 
maybeBoolToBool (Just x)  = x 
maybeBoolToBool Nothing   = False 

exists :: [Bool] -> Bool 
exists xs = unAny (foldr (<>) (MkAny False) (map (\x -> MkAny x) xs))

select :: Transformer -> Transformer
select t1 xs | exists boolList = [xs]
             | otherwise       = []
            where boolList    = map (\x -> maybeBoolToBool(getBool x)) transformed 
                  transformed = t1 xs 


greaterThan :: Transformer -> Transformer -> Transformer
greaterThan t1 t2 xs | elements xs == [] && (t1 xs) > (t2 xs) = [Boolean True]
                     | otherwise = [Boolean False]
                        where leftList  = [t1 a | a <- elements xs]
                              rightList = [t2 b | b <- elements xs]


lessThan :: Transformer -> Transformer -> Transformer
lessThan t1 t2 xs    | elements xs == [] && (t1 xs) < (t2 xs) = [Boolean True]
                     | otherwise = [Boolean False]
                        where leftList  = [t1 a | a <- elements xs]
                              rightList = [t2 b | b <- elements xs]

