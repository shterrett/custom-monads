module Free where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT (..), get, gets, put)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Utils

data Free f a
  = Free (f (Free f a))
  | Pure a

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Free fr) = Free $ (fmap f) <$> fr

instance Functor f => Applicative (Free f) where
  pure = Pure
  (Pure f) <*> fr = f <$> fr
  (Free f) <*> fr = Free $ (<*> fr) <$> f

instance Functor f => Monad (Free f) where
  return = Pure
  (Pure a) >>= f = f a
  (Free fr) >>= f = Free $ (>>= f) <$> fr

liftF :: (Functor f) => f a -> Free f a
liftF = Free . fmap pure

foldFree :: (Functor f, Monad m) => (forall x. f x -> m x) -> Free f a -> m a
foldFree _ (Pure a) = pure a
foldFree alg (Free fr) = alg fr >>= foldFree alg

data HangmanF r
  = GetInput (Text -> r)
  | ShowOutput Text r
  | TargetWord (Target -> r)
  | CorrectGuesses (Set Char -> r)
  | IncorrectGuesses (Set Char -> r)
  | PutCorrect Char r
  | PutIncorrect Char r
  | Threshold (Int -> r)
  | Done r
  deriving (Functor)

type Hangman = Free HangmanF

getInput :: Hangman Text
getInput = liftF $ GetInput id

showOutput :: Text -> Hangman ()
showOutput t = liftF $ ShowOutput t ()

targetWord :: Hangman Target
targetWord = liftF $ TargetWord id

correctGuesses :: Hangman (Set Char)
correctGuesses = liftF $ CorrectGuesses id

incorrectGuesses :: Hangman (Set Char)
incorrectGuesses = liftF $ IncorrectGuesses id

putCorrect :: Char -> Hangman ()
putCorrect c = liftF $ PutCorrect c ()

putIncorrect :: Char -> Hangman ()
putIncorrect c = liftF $ PutIncorrect c ()

threshold :: Hangman Int
threshold = liftF $ Threshold id

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

nat :: HangmanF a -> StateT GameState IO a
nat (GetInput k) = do
  input <- liftIO $ T.singleton <$> getChar
  pure (k input)
nat (ShowOutput t k) = do
  liftIO $ putStrLn $ T.unpack t
  pure k
nat (TargetWord k) = gets _target >>= pure . k
nat (CorrectGuesses k) = gets _correct >>= pure . k
nat (IncorrectGuesses k) = gets _incorrect >>= pure . k
nat (PutCorrect c k) = do
  s <- get
  put (s {_correct = S.insert c (_correct s)})
  pure k
nat (PutIncorrect c k) = do
  s <- get
  put (s {_incorrect = S.insert c (_incorrect s)})
  pure k
nat (Threshold k) = gets _threshold >>= pure . k
nat (Done a) = pure a

run :: Hangman a -> StateT GameState IO a
run = foldFree nat
