import System.Exit
import Test.QuickCheck 
import JSON 
import JSONOutput (renderJSON)



propReverse :: [Int] -> Bool 
propReverse xs = reverse (reverse xs) == xs 

-- JSON Output test --
renderJSONTest :: [JSON] -> Bool 
renderJSONTest xs = 



main :: IO ()
main = do quickCheck renderJSONTest 
