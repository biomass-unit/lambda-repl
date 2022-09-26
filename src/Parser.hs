module Parser where

import Control.Applicative (many, some, (<|>))
import Control.Monad       (void)
import Data.Char           (isSpace, isAlpha, isAlphaNum)
import Data.List           (foldl')

import qualified ParserCombinators as PC
import qualified AST


ws :: PC.Parser ()
ws = void (many (PC.pred isSpace ""))

wsd :: PC.Parser a -> PC.Parser a
wsd p = ws *> p <* ws

name :: PC.Parser String
name = (:)
  <$> PC.pred isAlpha "an identifier"
  <*> many (PC.pred isAlphaNum "")


variable :: PC.Parser AST.Expression
variable = AST.Variable
  <$> name
  <*  PC.negated (wsd (PC.char '=')) "a nested definition"

abstraction :: PC.Parser AST.Expression
abstraction = flip (foldr AST.Abstraction)
  <$> (ws *> PC.char '\\' *> ws *> some (wsd name))
  <*> (ws *> PC.char '.'  *> ws *> expression)

expression :: PC.Parser AST.Expression
expression = foldl' AST.Application <$> expr <*> many expr
  where expr = wsd (variable <|> abstraction <|> PC.parenthesized expression)


topLevelDefinition :: PC.Parser AST.TopLevel
topLevelDefinition = curry AST.TopLevelDefinition
  <$> name <* wsd (PC.char '=')
  <*> expression

topLevelExpression :: PC.Parser AST.TopLevel
topLevelExpression = AST.TopLevelExpression <$> expression

topLevel :: PC.Parser AST.TopLevel
topLevel = topLevelDefinition <|> topLevelExpression
