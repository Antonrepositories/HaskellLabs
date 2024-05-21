module Main where
import Data.List (sort)
import Data.List (nub)
import Data.List (groupBy, sortOn)
import Data.Function (on)

countUniqueOddElements :: [Int] -> Int
countUniqueOddElements lst = length . nub . filter odd $ lst

lastDigit :: Int -> Int
lastDigit n = n `mod` 10


generateM :: Int -> [Int]
generateM n = [k * (k + 1) `div` 2 | k <- [1..n]]


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

generateM1 :: Int -> [Int]
generateM1 n = [fib k | k <- [1..floor (fromIntegral n ** (1/3))]]


calcValue :: Int -> Int
calcValue k = round (fromIntegral k * sin (fromIntegral k) * cos (fromIntegral k))

generateSortedList :: Int -> [Int]
generateSortedList n = sort [calcValue k | k <- [1..n]]

transformList :: [a] -> [a]
transformList [] = []
transformList [x] = [x]
transformList xs = last xs : head xs : transformList (if length xs > 2 then init (tail xs) else [])

shiftLeft :: [a] -> [a]
shiftLeft [] = []
shiftLeft (x:xs) = xs ++ [x]


shiftRight :: Int -> [a] -> [a]
shiftRight k xs = take len . drop (len - k `mod` len) . cycle $ xs
  where len = length xs


valuePositions :: (Eq a, Ord a) => [a] -> [(a, [Int])]
valuePositions xs = map (\ys -> (fst (head ys), map snd ys)) grouped
  where
    indexed = zip xs [0..]
    grouped = groupBy ((==) `on` fst) $ sortOn fst indexed


moveHeadToEnd :: [a] -> [a]
moveHeadToEnd [] = []
moveHeadToEnd (x:xs) = xs ++ [x]

performNShifts :: Int -> [a] -> [a]
performNShifts 0 xs = xs
performNShifts n xs = performNShifts (n - 1) (moveHeadToEnd xs)

removeFirstN :: Int -> [a] -> [a]
removeFirstN n xs = drop n xs


factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)


sumFactorials :: Int -> Int
sumFactorials n = sum [factorial x | x <- [1..n]]


moveLastToFront :: [a] -> [a]
moveLastToFront [] = []
moveLastToFront [x] = [x]
moveLastToFront xs = last xs : init xs

performNShifts14 :: Int -> [a] -> [a]
performNShifts14 n xs = iterate moveLastToFront xs !! n


-- Тести
main :: IO ()
main = do
    putStrLn "1 task:"
    let test1 = [2, 4, 6, 8, 10]
    putStrLn $ "Test 1: " ++ show (countUniqueOddElements test1)
    let test2 = [1, 3, 5, 7, 9]
    putStrLn $ "Test 2: " ++ show (countUniqueOddElements test2)
    let test3 = [1, 2, 3, 4, 5, 1, 3, 5]
    putStrLn $ "Test 3: " ++ show (countUniqueOddElements test3)
    let test4 = []
    putStrLn $ "Test 4: " ++ show (countUniqueOddElements test4)
    let test5 = [7, 7, 7, 7]
    putStrLn $ "Test 5: " ++ show (countUniqueOddElements test5)
    putStrLn "2 task:"
    print $ lastDigit 123228
    
    print $ lastDigit 500

    print $ lastDigit 71
    putStrLn "3 task:"

    print $ generateM 5

    print $ generateM 0

    putStrLn "4 task:"
    print $ generateM1 1000
    print $ generateM1 2000
    putStrLn "6 task:"
    print $ generateSortedList 10
    print $ generateSortedList 0

    putStrLn "7 task:"
    print $ transformList [1, 2, 3, 4, 5, 6]
    print $ transformList [1, 2]
    print $ transformList ([] :: [Int])

    putStrLn "8 task:"
    print $ shiftLeft [1, 2, 3, 4, 5, 6]
    print $ shiftLeft [1, 2]
    print $ shiftLeft ([] :: [Int])

    putStrLn "9 task:"
    let k = 10  
    print $ shiftRight k [1, 2, 3, 4, 5, 6]
    print $ shiftRight k [1, 2]
    print $ shiftRight k ([] :: [Int])

    putStrLn "10 task:"
    print $ valuePositions [4, 6, 4, 8, 6, 4, 7, 8]
    print $ valuePositions [1, 2, 3, 4, 5, 6]
    print $ valuePositions ([] :: [Int])

    putStrLn "11 task:"
    let n = 3
    print $ performNShifts n [1, 2, 3, 4, 5, 6]
    print $ performNShifts n [1, 2]
    print $ performNShifts n ([] :: [Int])

    putStrLn "12 task:"
    print $ removeFirstN n [1, 2, 3, 4, 5, 6]
    print $ removeFirstN n [1, 2, 3, 4]
    print $ removeFirstN n ([] :: [Int])

    putStrLn "13 task:"
    let q = 10
    print $ sumFactorials q

    putStrLn "14 task:"
    print $ performNShifts14 n [1, 2, 3, 4, 5, 6, 7]
    print $ performNShifts14 n [1, 2]
    print $ performNShifts14 n ([] :: [Int])