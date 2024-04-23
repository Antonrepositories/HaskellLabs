import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub)
import Data.Char (isUpper) 
import Data.List


getUppercaseChars :: String -> [String]
getUppercaseChars str = map (:[]) $ filter isUpper str


-- Визначення структури для збереження лівої та правої частин рядка
data Line = Line { left :: String, right :: String } deriving (Show)

-- Функція для розбиття рядка за сепаратором та створення структури Line
parseLine :: String -> Line
parseLine str =
    let (leftPart, _:rightPart) = break (== '>') str
    in Line leftPart rightPart

-- Функція для зчитування файлу та розбиття кожного рядка на структуру Line
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
-- Приклад використання:
main :: IO ()
main = do
    lines <- readLinesFromFile "grammar.txt"
    mapM_ print lines
    putStrLn "Reachable non-terminals:"
    let rightValues = getRightByLeft "S" lines
    --print rightValues
    print $ findUnreachable "S" lines