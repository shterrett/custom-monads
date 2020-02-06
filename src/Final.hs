module Final where

import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Utils

class Hangman m where
  getInput :: m Text
  showOutput :: Text -> m ()
  targetWord :: m Target
  correctGuesses :: m (Set Char)
  incorrectGuesses :: m (Set Char)
  putCorrect :: Char -> m ()
  putIncorrect :: Char -> m ()
  threshold :: m Int

turn :: (Monad m, Hangman m) => m GuessResult
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

continueCorrect :: (Monad m, Hangman m) => Char -> m GuessResult
continueCorrect c = do
  putCorrect c
  winGame <$> targetWord <*> correctGuesses

continueIncorrect :: (Monad m, Hangman m) => GuessError -> m GuessResult
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
