import System.IO
import Text.Read (readMaybe)
import Data.List
import Data.Map (Map,empty,insert,lookup, (!))

parseInput :: String -> ([Int],[Int])
parseInput input = (sort (mapPairs parseInt),sort  (mapPairs (parseInt . tail)))
    where
        inputLines = lines input
        pairs = map words inputLines
        parseInt s = case readMaybe (head s) :: Maybe Int of
            Just num -> num
            Nothing -> 0
        mapPairs func = (map func pairs) 

diffInput :: ([Int],[Int]) -> [Int]
diffInput input = map subtractPair (zip (fst input) (snd input))
    where
        subtractPair x = abs ((fst x) - (snd x))

genMap :: [Int] -> Map Int Int
genMap input = foldl look Data.Map.empty input
    where 
        look :: Map Int Int -> Int -> Map Int Int
        look t i = case (Data.Map.lookup i t) of
            Just v -> Data.Map.insert i (v + 1) t
            Nothing -> Data.Map.insert i 1 t

findSum :: [Int] -> Map Int Int -> Int
findSum numbs m = sum (map calc numbs)
    where 
        calc x = case Data.Map.lookup x m of
            Just v -> x * v
            Nothing -> 0
        
 

main :: IO ()
main = do 
    fileHandle <- openFile "input_1_1" ReadMode
    contents <- hGetContents fileHandle
    let parseContents = parseInput contents 
    let diffs = diffInput parseContents
    let m = genMap (snd parseContents) 
    print (findSum (fst parseContents) m)
    hClose fileHandle
