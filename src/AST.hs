module AST (Expression(..)) where

import BU


data Expression
  = Abstraction String Expression
  | Application Expression Expression
  | Variable String
  deriving (Show, Eq)


instance PrettyShow Expression where
  prettyShow :: Expression -> String
  prettyShow = \case
    Abstraction parameter body ->
      "(\\" ++ parameter ++ "." ++ prettyShow body ++ ")"
    Application function argument ->
      prettyShow function ++ " " ++ prettyShow argument
    Variable name ->
      name
