{-# LANGUAGE GADTs #-}

module Coyoneda where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT (..), get, gets, put)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Free (Free (..), foldFree, liftF)
import Utils

data Coyoneda f a where
  Coyoneda :: (b -> a) -> f b -> Coyoneda f a

instance Functor (Coyoneda f) where
  fmap f (Coyoneda g fb) = Coyoneda (f . g) fb

type Freer f = Free (Coyoneda f)

data HangmanI a where
  GetInput :: HangmanI Text
  ShowOutput :: Text -> HangmanI ()
  TargetWord :: HangmanI Target
  CorrectGuesses :: HangmanI (Set Char)
  IncorrectGuesses :: HangmanI (Set Char)
  PutCorrect :: Char -> HangmanI ()
  PutIncorrect :: Char -> HangmanI ()
  Threshold :: HangmanI Int

type Hangman a = Freer HangmanI a

liftFr :: f a -> Freer f a
liftFr = liftF . Coyoneda id

getInput :: Hangman Text
getInput = liftFr GetInput

showOutput :: Text -> Hangman ()
showOutput t = liftFr (ShowOutput t)

targetWord :: Hangman Target
targetWord = liftFr TargetWord

correctGuesses :: Hangman (Set Char)
correctGuesses = liftFr CorrectGuesses

incorrectGuesses :: Hangman (Set Char)
incorrectGuesses = liftFr IncorrectGuesses

putCorrect :: Char -> Hangman ()
putCorrect c = liftFr (PutCorrect c)

putIncorrect :: Char -> Hangman ()
putIncorrect c = liftFr (PutIncorrect c)

threshold :: Hangman Int
threshold = liftFr Threshold

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

nat :: HangmanI a -> StateT GameState IO a
nat GetInput = liftIO $ T.singleton <$> getChar
nat (ShowOutput t) = liftIO $ putStrLn $ T.unpack t
nat TargetWord = gets _target
nat CorrectGuesses = gets _correct
nat IncorrectGuesses = gets _incorrect
nat (PutCorrect c) = do
  s <- get
  put (s {_correct = S.insert c (_correct s)})
nat (PutIncorrect c) = do
  s <- get
  put (s {_incorrect = S.insert c (_incorrect s)})
nat Threshold = gets _threshold

runCoyoneda :: Monad m => (forall x. f x -> m x) -> Coyoneda f a -> m a
runCoyoneda alg (Coyoneda fn fa) = fn <$> alg fa

run :: Hangman a -> StateT GameState IO a
run = foldFree (runCoyoneda nat)
