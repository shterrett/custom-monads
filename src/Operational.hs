{-# LANGUAGE GADTs #-}

module Operational where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT (..), get, gets, put)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Utils

data Hangman a where
  GetInput :: Hangman Text
  ShowOutput :: Text -> Hangman ()
  TargetWord :: Hangman Target
  CorrectGuesses :: Hangman (Set Char)
  IncorrectGuesses :: Hangman (Set Char)
  PutCorrect :: Char -> Hangman ()
  PutIncorrect :: Char -> Hangman ()
  Threshold :: Hangman Int
  Done :: a -> Hangman a
  Bind :: Hangman a -> (a -> Hangman b) -> Hangman b

instance Functor Hangman where
  fmap f h = h `Bind` (\a -> Done $ f a)

instance Applicative Hangman where
  pure = Done
  f <*> x = f `Bind` (\fx -> x `Bind` (\x' -> Done (fx x')))

instance Monad Hangman where
  return = Done
  (>>=) = Bind

getInput :: Hangman Text
getInput = GetInput

showOutput :: Text -> Hangman ()
showOutput = ShowOutput

targetWord :: Hangman Target
targetWord = TargetWord

correctGuesses :: Hangman (Set Char)
correctGuesses = CorrectGuesses

incorrectGuesses :: Hangman (Set Char)
incorrectGuesses = IncorrectGuesses

putCorrect :: Char -> Hangman ()
putCorrect = PutCorrect

putIncorrect :: Char -> Hangman ()
putIncorrect = PutIncorrect

threshold :: Hangman Int
threshold = Threshold

done :: a -> Hangman a
done = Done

bind :: Hangman a -> (a -> Hangman b) -> Hangman b
bind = Bind

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
run GetInput = liftIO $ T.singleton <$> getChar
run (ShowOutput t) = liftIO $ putStrLn $ T.unpack t
run TargetWord = gets _target
run CorrectGuesses = gets _correct
run IncorrectGuesses = gets _incorrect
run (PutCorrect c) = do
  s <- get
  put (s {_correct = S.insert c (_correct s)})
run (PutIncorrect c) = do
  s <- get
  put (s {_incorrect = S.insert c (_incorrect s)})
run Threshold = gets _threshold
run (Done a) = pure a
run (Bind h f) = run (h >>= f)
