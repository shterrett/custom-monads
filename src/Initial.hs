module Initial where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT (..), get, gets, put)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Utils

data Hangman a
  = GetInput (Text -> Hangman a)
  | ShowOutput Text (Hangman a)
  | TargetWord (Target -> Hangman a)
  | CorrectGuesses (Set Char -> Hangman a)
  | IncorrectGuesses (Set Char -> Hangman a)
  | PutCorrect Char (Hangman a)
  | PutIncorrect Char (Hangman a)
  | Threshold (Int -> Hangman a)
  | Done a
  deriving (Functor)

instance Applicative Hangman where
  pure = Done
  GetInput k <*> h = GetInput (\t -> k t <*> h)
  ShowOutput t k <*> h = ShowOutput t (k <*> h)
  TargetWord k <*> h = TargetWord (\t -> k t <*> h)
  CorrectGuesses k <*> h = CorrectGuesses (\s -> k s <*> h)
  IncorrectGuesses k <*> h = IncorrectGuesses (\s -> k s <*> h)
  PutCorrect c k <*> h = PutCorrect c (k <*> h)
  PutIncorrect c k <*> h = PutIncorrect c (k <*> h)
  Threshold k <*> h = Threshold (\i -> k i <*> h)
  Done f <*> h = f <$> h

instance Monad Hangman where
  return = Done
  GetInput k >>= f = GetInput (\t -> k t >>= f)
  ShowOutput t k >>= f = ShowOutput t (k >>= f)
  TargetWord k >>= f = TargetWord (\t -> k t >>= f)
  CorrectGuesses k >>= f = CorrectGuesses (\s -> k s >>= f)
  IncorrectGuesses k >>= f = IncorrectGuesses (\s -> k s >>= f)
  PutCorrect c k >>= f = PutCorrect c (k >>= f)
  PutIncorrect c k >>= f = PutIncorrect c (k >>= f)
  Threshold k >>= f = Threshold (\i -> k i >>= f)
  Done a >>= f = f a

getInput :: Hangman Text
getInput = GetInput pure

showOutput :: Text -> Hangman ()
showOutput t = ShowOutput t (pure ())

targetWord :: Hangman Target
targetWord = TargetWord pure

correctGuesses :: Hangman (Set Char)
correctGuesses = CorrectGuesses pure

incorrectGuesses :: Hangman (Set Char)
incorrectGuesses = IncorrectGuesses pure

putCorrect :: Char -> Hangman ()
putCorrect c = PutCorrect c (pure ())

putIncorrect :: Char -> Hangman ()
putIncorrect c = PutIncorrect c (pure ())

threshold :: Hangman Int
threshold = Threshold pure

turn :: Hangman GuessResult
turn = do
  t <- targetWord
  correct <- correctGuesses
  showOutput $ showTarget t correct
  incorrect <- incorrectGuesses
  showOutput $ showIncorrect incorrect
  guess <- showOutput "Next Guess: " >> getInput
  let guessResult = isChar guess >>= alreadyGuessed correct incorrect >>= checkGuess t
  case guessResult of
    Right c -> continueCorrect c
    Left err -> continueIncorrect err

continueCorrect :: Char -> Hangman GuessResult
continueCorrect c = do
  putCorrect c
  winGame <$> targetWord <*> correctGuesses

continueIncorrect :: GuessError -> Hangman GuessResult
continueIncorrect err = do
  case err of
    Incorrect c -> do
      putIncorrect c
      showOutput $ T.singleton c <> " is not in the word"
    AlreadyGuessed c ->
      showOutput $ T.singleton c <> " has already been guessed"
    NotChar ->
      showOutput "Please enter a character between a and z"
  loseGame <$> incorrectGuesses

run :: Hangman a -> StateT GameState IO a
run (GetInput k) = do
  input <- liftIO $ T.singleton <$> getChar
  run (k input)
run (ShowOutput t k) = do
  liftIO $ putStrLn $ T.unpack t
  run k
run (TargetWord k) = gets _target >>= run . k
run (CorrectGuesses k) = gets _correct >>= run . k
run (IncorrectGuesses k) = gets _incorrect >>= run . k
run (PutCorrect c k) = do
  s <- get
  put (s {_correct = S.insert c (_correct s)})
  run k
run (PutIncorrect c k) = do
  s <- get
  put (s {_incorrect = S.insert c (_incorrect s)})
  run k
run (Threshold k) = gets _threshold >>= run . k
run (Done a) = pure a
