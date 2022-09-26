import System.Process (system)
import System.IO      (hFlush, stdout)
import Data.Char      (isSpace)

import qualified BU
import qualified AST
import qualified Parser
import qualified ParserCombinators as PC


data State
  = State
  { previousDirective :: String
  , currentBindings   :: [(String, AST.Expression)]
  }


helpText :: String
helpText =
  "lambda-repl accepts three kinds of input:                      \n\
  \  1 An expression to be evaluated                              \n\
  \  2 A definition, with the following syntax: name = expression \n\
  \  3 A REPL directive                                           \n\
  \                                                               \n\
  \Here's a list of valid REPL directives:                        \n\
  \  :         Run the previous directive                         \n\
  \  :! cmd    Run cmd in the current shell                       \n\
  \  :?        Show this help-text                                \n\
  \  :d name   Show the definition of name                        \n\
  \  :q        Exit lambda-repl                                   \n"


stripSpace :: String -> String
stripSpace = f . f where f = dropWhile isSpace . reverse


runDirective :: State -> String -> IO ()
runDirective oldState directive =
  let newState = oldState { previousDirective = directive }
  in case stripSpace directive of
    ""  -> runDirective oldState (previousDirective oldState)
    "?" -> do
      putStr helpText
      repl newState
    "!" -> do
      putStrLn "Use ':! cmd' to run cmd in the current shell"
      repl oldState
    '!':cmd -> do
      exitCode <- system cmd
      print exitCode
      repl newState
    "q" -> do
      putStrLn "Leaving lambda-repl. Goodbye!"
      pure ()
    "d" -> do
      putStrLn "Use ':d name' to show the definition of name"
      repl oldState
    'd':' ':name ->
      case name `lookup` currentBindings oldState of
        Just expression -> do
          putStrLn (name ++ " = " ++ BU.prettyShow expression)
          repl newState
        Nothing -> do
          putStrLn "There is no definition for the given name"
          repl newState
    other -> do
      putStrLn ('\'' : other ++ "' is not a valid REPL directive. \
        \(For help, enter :?)")
      repl oldState


repl :: State -> IO ()
repl state = do
  putStr "> "
  hFlush stdout -- Ensure the prompt is printed before getLine is called
  input <- getLine
  case stripSpace input of
    ""              -> repl state
    (':':directive) -> runDirective state directive
    input -> case PC.parse Parser.topLevel (PC.makeInput input) of
      Right (topLevel, _) -> case topLevel of
        AST.TopLevelDefinition pair -> do
          repl state { currentBindings = pair : currentBindings state }
        AST.TopLevelExpression _ -> do
          putStrLn "Unimplemented"
          repl state
      Left _ -> do
        putStrLn "Parse error"
        repl state


main :: IO ()
main = do
  putStrLn "Welcome to lambda-repl! Enter :? for help."
  repl State
    { previousDirective = "?"
    , currentBindings   = []
    }
