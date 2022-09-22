module ParserCombinators
  ( Position
  , Input
  , Error
  , Parser
  ) where


data Position
  = Position
  { line   :: Int
  , column :: Int
  } deriving (Show, Eq)

data Input
  = Input
  { position  :: Position
  , remaining :: String
  } deriving (Show, Eq)

data Error
  = Error
  { position :: Position
  , message  :: String
  }

newtype Parser a
  = Parser
  { parse :: Input -> (a, Input)
  }
