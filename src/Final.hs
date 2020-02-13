module Final where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State (StateT (..), gets, modify)
import Data.Set (Set)
import qualified Data.Set as S
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

data GameState
  = GameState
      { _target :: Target,
        _correct :: Set Char,
        _incorrect :: Set Char,
        _threshold :: Int
      }

newtype Runner a = Runner {unRunner :: StateT GameState IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance Hangman Runner where
  getInput = T.singleton <$> liftIO getChar
  showOutput = liftIO . putStrLn . T.unpack
  targetWord = Runner $ gets _target
  correctGuesses = Runner $ gets _correct
  incorrectGuesses = Runner $ gets _incorrect
  putCorrect c = Runner $ modify (\s -> s {_correct = S.insert c (_correct s)})
  putIncorrect c = Runner $ modify (\s -> s {_incorrect = S.insert c (_incorrect s)})
  threshold = Runner $ gets _threshold
