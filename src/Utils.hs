module Utils where

import Data.Char (isAlpha, toLower)
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

newtype Target = Target { unTarget :: Text }

data GuessError =
    Incorrect Char
    | AlreadyGuessed Char
    | NotChar
    deriving (Eq, Show)

data GuessResult =
    Continue
    | Win
    | Lose
    deriving (Show, Eq)

showTarget :: Target -> Set Char -> Text
showTarget (Target t) correct =
    mconcat $ obfuscate <$> T.unpack t
  where obfuscate c = if Set.member c correct
                        then T.singleton c
                        else unknown

showIncorrect :: Set Char -> Text
showIncorrect incorrect =
    "Previous Guesses: " <> guesses
  where guesses = T.intersperse ' ' (T.pack $ sort $ Set.elems incorrect)

unknown :: Text
unknown = "▮"

isChar :: Text -> Either GuessError Char
isChar = toEither . T.unpack
  where toEither = \case
                     c:"" | isAlpha c -> Right $ toLower c
                          | otherwise -> Left NotChar
                     _ -> Left NotChar

alreadyGuessed ::
  Set Char
  -> Set Char
  -> Char
  -> Either GuessError Char
alreadyGuessed correct incorrect c =
    if Set.member c (Set.union correct incorrect)
      then Left $ AlreadyGuessed c
      else Right c

checkGuess :: Target -> Char -> Either GuessError Char
checkGuess (Target t) c =
    if T.any (== c) t
      then Right c
      else Left $ Incorrect c

loseGame :: Set Char -> GuessResult
loseGame incorrect =
    if Set.size incorrect >= maxMisses
      then Lose
      else Continue

winGame :: Target -> Set Char -> GuessResult
winGame (Target t) correct = if correct == haystack
              then Win
              else Continue
  where haystack = Set.fromList $ T.unpack t

maxMisses :: Int
maxMisses = 5
