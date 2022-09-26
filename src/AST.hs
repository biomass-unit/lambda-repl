module AST where

import BU


data Expression
  = Abstraction String Expression
  | Application Expression Expression
  | Variable String
  deriving (Show, Eq)

data TopLevel
  = TopLevelExpression Expression
  | TopLevelDefinition (String, Expression)
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
