import Data.Maybe (Maybe(..))
import Control.Monad (liftM2)

-- Функція f1
f1 :: Float -> Maybe Float
f1 x = if x^2 - 9 <= 0 || x^2 - 9 == 1 then Nothing else Just (1 / logBase 10 (x^2 - 9))

-- Функція f2
f2 :: Float -> Maybe Float
f2 x = if x - 1/9 <= 0 then Nothing else Just (logBase 10 (x - 1/9))

-- Функція f3
f3 :: Float -> Maybe Float
f3 x = if x - 1/9 <= 0 then Nothing else Just (sqrt (x - 1/9))


superpositiondo :: Float -> Maybe Float
superpositiondo x = do
    resultF3 <- f3 x
    resultF2 <- f2 resultF3
    resultF1 <- f1 resultF2
    return resultF1


superposition :: Float -> Maybe Float
superposition x = f3 x >>= \resultF3 ->
                   f2 resultF3 >>= \resultF2 ->
                   f1 resultF2


f3b :: Float -> Float -> Maybe Float
f3b x n = if n == 0 || x - 1/n <= 0 then Nothing else Just (sqrt (x - 1/n))


superposition2do :: (Float -> Maybe Float) -> (Float -> Maybe Float) -> Float -> Maybe Float
superposition2do f g x = do
    resultF <- f x
    resultG <- g x
    resultF3B <- f3b resultF resultG
    return resultF3B

superposition2 :: (Float -> Maybe Float) -> (Float -> Maybe Float) -> Float -> Maybe Float
superposition2 f g x = f x >>= \resultF1 ->
                   g x >>= \resultF2 ->
                   f3b resultF1 resultF2





main :: IO ()
main = do
  -- Позитивні тести
  putStrLn "Positive tests:"
  putStrLn "-----------------"
  testFunction "f1" f1 [20, 20.001, 5]
  testFunction "f2" f2 [19.01, 20, 20.001]
  testFunction "f3" f3 [19.01, 20, 20.001]
  -- Негативні тести
  putStrLn "Negative tests:"
  putStrLn "-----------------"
  testFunction "f1" f1 [0, 1, sqrt 10]
  testFunction "f2" f2 [0, 1/9, 0.01]
  testFunction "f3" f3 [0, 1/9, 0.01]
  --Суперпозиція
  putStrLn "Testing superposition with do-notation:"
  print $ superpositiondo 2000000 -- Повинно вивести Just як результат
  print $ superpositiondo 0 -- Повинно вивести Nothing як результат
  putStrLn "Testing superposition without do-notation:"
  print $ superposition 2000000 -- Повинно вивести Just як результат
  print $ superposition 0 -- Повинно вивести Nothing як результат
  -- бінарна 
  putStrLn "Testing binary f3:"
  print $ f3b 10 9 -- Повинно вивести Just, оскільки sqrt (x - 1/n) визначено
  print $ f3b 2 0 -- Повинно вивести Nothing, оскільки n == 0
  print $ f3b 1 1 -- Повинно вивести Nothing, оскільки sqrt (x - 1/n) не визначено
  --Суперпозиція
  putStrLn "Testing superposition2 with do-notation:"
  print $ superposition2do f1 f3 5000  -- Повинно вивести Just як результат
  print $ superposition2do f1 f3 0-- Повинно вивести Nothing як результат
  putStrLn "Testing superposition2 without do-notation:"
  print $ superposition2 f1 f3 5000  -- Повинно вивести Just як результат
  print $ superposition2 f1 f3 0-- Повинно вивести Nothing як результат

-- Функція для тестування
testFunction :: String -> (Float -> Maybe Float) -> [Float] -> IO ()
testFunction name f xs = do
  putStrLn $ "Testing " ++ name ++ ":"
  mapM_ (\x -> putStrLn $ "  " ++ name ++ "(" ++ show x ++ ") = " ++ show (f x)) xs

