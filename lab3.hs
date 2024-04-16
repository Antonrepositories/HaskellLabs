import Data.Set (Set)
import qualified Data.Set as Set
import System.IO


data Transition = Transition
  { currentState :: String,
    inputSymbol :: String,
    nextState :: String
  }
  deriving (Show)

data Automaton = Automaton
  { transitions :: [Transition],
    initialState :: String,
    finalStates :: Set String
  }
  deriving (Show)


parseTransition :: String -> Transition
parseTransition line =
  let [from, input, to] = words line
   in Transition from input to

readTransitions :: FilePath -> IO [Transition]
readTransitions filePath = do
  contents <- readFile filePath
  return $ map parseTransition (lines contents)


readFinalStates :: FilePath -> IO (Set.Set String)
readFinalStates filePath = do
  contents <- readFile filePath
  return $ Set.fromList (words contents)

readAlphabet :: FilePath -> IO String
readAlphabet filePath = do
  contents <- readFile filePath
  return contents

isAccepted :: Automaton -> String -> Bool
isAccepted automaton word = go (initialState automaton) word
  where
    go :: String -> String -> Bool
    go state [] = Set.member state (finalStates automaton)
    go state (x : xs) =
      case filter (\t -> currentState t == state && inputSymbol t == [x]) (transitions automaton) of
        [] -> False
        ts -> any (\t -> go (nextState t) xs) ts


isEvenLength :: String -> Bool
isEvenLength str = even (length str)


generateEvenLengthWords :: String -> Int -> [String]
generateEvenLengthWords alphabet mxln = [w | len <- [2, 4 .. mxln], w <- sequence (replicate len alphabet)]

acceptsEvenLength :: Automaton -> String -> Int -> Bool
acceptsEvenLength automaton alphabet mxln = any (isAccepted automaton) (generateEvenLengthWords alphabet mxln) -- Modify alphabet as needed

main :: IO ()
main = do
  alphabet <- readAlphabet "alphabet.txt"
  transitions1 <- readTransitions "transitions.txt"
  finalStates1 <- readFinalStates "finalstates2.txt"
  let automaton = Automaton
        { transitions = transitions1,
          initialState = "0",
          finalStates = finalStates1 
        }
      maxLen = length transitions1 + 5
      acceptsEven = acceptsEvenLength automaton alphabet maxLen
  putStrLn $ "Does the automaton accept at least one word of even length? " ++ show acceptsEven