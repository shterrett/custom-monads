{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CoproductOfFunctors where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT (..), get, gets, put)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Free (Free (..), foldFree, liftF)
import Utils

data (f :+: g) a = InL (f a) | InR (g a)
  deriving (Functor)

class f :<: g where
  inject :: f a -> g a

instance f :<: f where
  inject = id

instance f :<: (f :+: g) where
  inject = InL

instance (f :<: h) => f :<: (g :+: h) where
  inject = InR . inject

data HangmanIOF r
  = GetInput (Text -> r)
  | ShowOutput Text r
  | IODone r
  deriving (Functor)

data HangmanStateF r
  = TargetWord (Target -> r)
  | CorrectGuesses (Set Char -> r)
  | IncorrectGuesses (Set Char -> r)
  | PutCorrect Char r
  | PutIncorrect Char r
  | Threshold (Int -> r)
  | StateDone r
  deriving (Functor)

liftEff :: (Functor f, Functor g, (f :<: g)) => f a -> Free g a
liftEff = liftF . inject

getInput :: (Functor f, (HangmanIOF :<: f)) => Free f Text
getInput = liftEff $ GetInput id

showOutput :: (Functor f, (HangmanIOF :<: f)) => Text -> Free f ()
showOutput t = liftEff $ ShowOutput t ()

targetWord :: (Functor f, (HangmanStateF :<: f)) => Free f Target
targetWord = liftEff $ TargetWord id

correctGuesses :: (Functor f, (HangmanStateF :<: f)) => Free f (Set Char)
correctGuesses = liftEff $ CorrectGuesses id

incorrectGuesses :: (Functor f, (HangmanStateF :<: f)) => Free f (Set Char)
incorrectGuesses = liftEff $ IncorrectGuesses id

putCorrect :: (Functor f, (HangmanStateF :<: f)) => Char -> Free f ()
putCorrect c = liftEff $ PutCorrect c ()

putIncorrect :: (Functor f, (HangmanStateF :<: f)) => Char -> Free f ()
putIncorrect c = liftEff $ PutIncorrect c ()

threshold :: (Functor f, (HangmanStateF :<: f)) => Free f Int
threshold = liftEff $ Threshold id

turn :: (Functor f, (HangmanStateF :<: f), (HangmanIOF :<: f)) => Free f GuessResult
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

continueCorrect :: (Functor f, (HangmanIOF :<: f), (HangmanStateF :<: f)) => Char -> Free f GuessResult
continueCorrect c = do
  putCorrect c
  winGame <$> targetWord <*> correctGuesses

continueIncorrect :: (Functor f, (HangmanIOF :<: f), (HangmanStateF :<: f)) => GuessError -> Free f GuessResult
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

natIO :: HangmanIOF a -> StateT GameState IO a
natIO (GetInput k) = do
  input <- liftIO $ T.singleton <$> getChar
  pure (k input)
natIO (ShowOutput t k) = do
  liftIO $ putStrLn $ T.unpack t
  pure k
natIO (IODone a) = pure a

natS :: HangmanStateF a -> StateT GameState IO a
natS (TargetWord k) = gets _target >>= pure . k
natS (CorrectGuesses k) = gets _correct >>= pure . k
natS (IncorrectGuesses k) = gets _incorrect >>= pure . k
natS (PutCorrect c k) = do
  s <- get
  put (s {_correct = S.insert c (_correct s)})
  pure k
natS (PutIncorrect c k) = do
  s <- get
  put (s {_incorrect = S.insert c (_incorrect s)})
  pure k
natS (Threshold k) = gets _threshold >>= pure . k
natS (StateDone a) = pure a

combine :: (f a -> m a) -> (g a -> m a) -> (f :+: g) a -> m a
combine f _ (InL fa) = f fa
combine _ g (InR ga) = g ga

run :: Free (HangmanIOF :+: HangmanStateF) a -> StateT GameState IO a
run = foldFree (combine natIO natS)
