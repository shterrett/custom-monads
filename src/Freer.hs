{-# LANGUAGE GADTs #-}

module Freer where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT (..), get, gets, put)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Utils

data Freer f a where
  Pure :: a -> Freer f a
  Bind :: Freer f a -> (a -> Freer f b) -> Freer f b
  Freer :: f a -> Freer f a

instance Functor (Freer f) where
  fmap fn freer = freer `Bind` (\a -> Pure $ fn a)

instance Applicative (Freer f) where
  pure = Pure
  fn <*> fa = fn `Bind` (\f -> fa `Bind` (\a -> Pure $ f a))

instance Monad (Freer f) where
  return = Pure
  (>>=) = Bind

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

getInput :: Hangman Text
getInput = Freer GetInput

showOutput :: Text -> Hangman ()
showOutput = Freer . ShowOutput

targetWord :: Hangman Target
targetWord = Freer TargetWord

correctGuesses :: Hangman (Set Char)
correctGuesses = Freer CorrectGuesses

incorrectGuesses :: Hangman (Set Char)
incorrectGuesses = Freer IncorrectGuesses

putCorrect :: Char -> Hangman ()
putCorrect = Freer . PutCorrect

putIncorrect :: Char -> Hangman ()
putIncorrect = Freer . PutIncorrect

threshold :: Hangman Int
threshold = Freer Threshold

done :: a -> Hangman a
done = Pure

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

foldFreer :: (Monad m) => (forall x. f x -> m x) -> Freer f a -> m a
foldFreer _ (Pure a) = pure a
foldFreer alg (Bind fa fn) = foldFreer alg fa >>= foldFreer alg . fn
foldFreer alg (Freer fa) = alg fa

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

run :: Hangman a -> StateT GameState IO a
run = foldFreer nat
