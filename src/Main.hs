import Data.Char (isSpace)

data State
  = State
  { previousDirective :: String
  }


helpText :: String
helpText =
  ":  Run the previous directive\n\
  \:? Show this help-text       \n\
  \:q Exit lambda-repl          \n"


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
    "q" -> do
      putStrLn "Leaving lambda-repl. Goodbye!"
      pure ()
    other -> do
      putStrLn ('\'' : other ++ "' is not a valid REPL directive. \
        \(For help, enter :?)")
      repl oldState


repl :: State -> IO ()
repl state = putStr "> " >> getLine >>= (. stripSpace) \case
  ""              -> repl state
  (':':directive) -> runDirective state directive
  _ -> undefined


main :: IO ()
main = do
  putStrLn "Welcome to lambda-repl! Enter :? for help."
  repl State { previousDirective = "?" }
