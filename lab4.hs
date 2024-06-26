import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub)
import Data.Char (isUpper) 
import Data.List


getUppercaseChars :: String -> [String]
getUppercaseChars str = map (:[]) $ filter isUpper str


data Line = Line { left :: String, right :: String } deriving (Show)

parseLine :: String -> Line
parseLine str =
    let (leftPart, _:rightPart) = break (== '>') str
    in Line leftPart rightPart

readLinesFromFile :: FilePath -> IO [Line]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return $ map parseLine (lines contents)


getRightByLeft :: String -> [Line] -> [String]
getRightByLeft x lines = [right line | line <- lines, left line == x]





findUnreachable :: String -> [Line] -> [String]
findUnreachable start lines = nub $ findUnreachable' [start] [] where
    findUnreachable' [] visited = visited
    findUnreachable' (x:xs) visited
        | x `elem` visited = findUnreachable' xs visited
        | otherwise = let newVisited = x : visited
                          relevantLines = getRightByLeft x lines
                          newNonTerminals = nub $ filter (`notElem` newVisited) $ concatMap getUppercaseChars relevantLines
                      in findUnreachable' (xs ++ newNonTerminals) newVisited

main :: IO ()
main = do
    lines <- readLinesFromFile "grammar.txt"
    mapM_ print lines
    let firstElem = left (head lines)
    putStrLn "All non-terminals:"
    let allNonterminals = nub $ map left lines
    print allNonterminals
    putStrLn "Reachable non-terminals:"
    --let reachableNonterminals = findUnreachable "S" lines
    let reachableNonterminals = findUnreachable firstElem lines
    print $ reachableNonterminals
    --print $ findUnreachable "S" lines
    putStrLn "Unreachable non-terminals:"
    let unreachableNonterminals = filter (`notElem` reachableNonterminals) allNonterminals
    print unreachableNonterminals
    let filteredLines = filter (\line -> left line `elem` reachableNonterminals) lines
    putStrLn "New grammar:"
    mapM_ print filteredLines