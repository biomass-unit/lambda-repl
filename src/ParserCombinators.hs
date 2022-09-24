module ParserCombinators
( Position(..)
, Input   (..)
, Error   (..)
, Parser  (..)
, makeInput
, pred
, char
, string
, digit
, digits
, int
, parenthesized
) where

import Prelude      hiding (pred)
import Data.Foldable       (traverse_)
import Data.Char           (isDigit)
import Control.Applicative (Alternative(..))


data Position
  = Position
  { line   :: Int
  , column :: Int
  } deriving (Show, Eq)

data Input
  = Input Position String
  deriving (Show, Eq)

data Error
  = Error
  { position :: Position
  , message  :: String
  } deriving (Show, Eq)

newtype Parser a
  = Parser
  { parse :: Input -> Either Error (a, Input)
  }


makeInput :: String -> Input
makeInput = Input Position { line = 1, column = 1 }

instance Ord Position where
  (<=) :: Position -> Position -> Bool
  a <= b = (line a, column a) <= (line b, column b)


instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser \i -> do
    (x, i) <- parse p i
    Right (f x, i)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser \i -> Right (x, i)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = Parser \i -> do
    (f, i) <- parse pf i
    (x, i) <- parse px i
    Right (f x, i)

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser \i -> do
    (x, i) <- parse p i
    parse (f x) i

instance Alternative Parser where
  empty :: Parser a
  empty = undefined

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser \i -> case (parse p1 i, parse p2 i) of
    (Right l, Left _) -> Right l
    (Left _, Right r) -> Right r
    (Right l@(_, Input pos1 _), Right r@(_, Input pos2 _)) ->
      Right if pos1 >= pos2 then l else r
    (e, _) -> e -- FIX

advanceWith :: Position -> Char -> Position
advanceWith pos = \case
  '\n' -> Position { line = line pos + 1, column = 1 }
  _    -> pos { column = column pos + 1 }

quote :: String -> String
quote s = '\'' : s ++ "\'"

summarize :: String -> String
summarize = quote . take 10

pred :: (Char -> Bool) -> String -> Parser Char
pred predicate description
  = Parser \(Input pos str) ->
    let errorBecause = Left . Error pos . (("Expected " ++ description ++ ", but found ") ++)
    in case str of
      []                   -> errorBecause "the end of input"
      (c:cs) | predicate c -> Right (c, Input (pos `advanceWith` c) cs)
             | otherwise   -> errorBecause (summarize str)

char :: Char -> Parser Char
char c = pred (== c) ['\'', c, '\'']

string :: String -> Parser String
string s = s <$ traverse_ char s

digit :: Parser Char
digit = pred isDigit "a digit"

digits :: Parser String
digits = some digit

int :: Parser Integer
int = read <$> digits

parenthesized :: Parser a -> Parser a
parenthesized p = char '(' *> p <* char ')'
