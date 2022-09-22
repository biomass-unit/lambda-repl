module AST (Expression(..)) where


data Expression
  = Abstraction String Expression
  | Application Expression Expression
  | Variable String
  deriving Eq


instance Show Expression where
  show :: Expression -> String
  show = \case
    Abstraction parameter body ->
      "(\\" ++ parameter ++ "." ++ show body ++ ")"
    Application function argument ->
      show function ++ " " ++ show argument
    Variable name ->
      name
