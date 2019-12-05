# custom-monads

Multiple implementations of a single "turn" of _Hangman_ using custom monadic
interpreters:

+ Final (tagless)
+ Initial (tagged)
+ Free Monads
+ Free-er Monads

Based on material presented in _Book Of Monads_ by Alejandro Serrano Mena

# Hangman

In an arbitrary turn of hangman:
+ Display the (partially) obfuscated word
+ Display the previously missed guesses
+ Ask user for a guess
+ Validate that the guess has not been guessed already
+ Check if the guess is correct
+ If incorrect, check if the number of missed guesses is greater than some
  arbitrary threshold; if so, then indicate the game is over
+ If correct, check if the word is solved; if so, indicate the game is over

Basic operations:
+ Get Input
+ Show output
+ Get target word
+ Get correct guesses
+ Put correct guess
+ Get incorrect guesses
+ Put incorrect guess
+ Get game-over threshold (or hardcode)
