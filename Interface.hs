import Parser
import Type
import SLD (dfs, bfs, solve)
import Subst (Subst, pretty, empty)

-- starts the REPL interpreter of prolog
nippl :: IO ()
nippl = do
  putStrLn ("Welcome! \nType :h for help.")
  loop "" (Prog [])
  return ()

-- loop for the interactive program
loop :: String -> Prog -> IO ()
loop str p  = do
  putStr "?- "
  input  <- getLine
  if input == ":h"
    then do
      help
      loop str p
    else if (take 2 input) == ":l"
        then do
          load input str p
        else if (take 2 input) == ":s"
          then do
            strategy input str p
          else if not (null input) &&
                  last input == '.' &&
                  str /= "" &&
                  (\(Prog prog) -> not (null prog)) p
            then do
              results str input p
            else if input == ":q"
              then return ()
              else do
                putStrLn "|"
                loop str p

-- if ":h" was typed, display help commands
help :: IO ()
help = do
  putStrLn "Commands available from the prompt:"
  putStrLn "  <goal>      Solves/proves the specified goal."
  putStrLn "  :h          Shows this help message."
  putStrLn "  :l <file>   Loads the specified file."
  putStrLn "  :q          Exits the interactive environment."
  putStrLn "  :s <strat>  Sets the specified search strategy"
  putStrLn "              where <strat> is one of 'dfs', 'bfs'."

-- if ":l" was typed, load the following file
load :: String -> String -> Prog -> IO ()
load input str p = do
  eFile <- (parseFile (drop 3 input) :: IO (Either String Prog))
  case eFile of
    (Left err)   -> do
      putStrLn err
      loop str p
    (Right prog) -> do
      putStrLn "Loaded."
      loop str prog

-- if a goal was typed
results :: String -> String -> Prog -> IO ()
results str g p = do
  case (parse g :: (Either String Goal)) of
    (Left err)   -> do
      putStrLn err
      loop str p
    (Right goal) -> do
      if str == "bfs"
        then do
          let subst = solve bfs p goal
          listLoop subst
          loop str p
        else do
          let subst = solve dfs p goal
          listLoop subst
          loop str p

-- loop to display all results for a goal
listLoop :: [Subst] -> IO ()
listLoop [] = do
  putStrLn "No more solutions."
  return ()
listLoop (s:ss) = do
  if pretty s == pretty empty
    then do
      putStrLn "True."
      return ()
    else do
      putStr (pretty s)
      user <- getLine
      if user == ";"
        then do
          listLoop ss
        else do
          return ()

-- if ":s" was typed, change the type to either bfs or dfs
strategy :: String -> String -> Prog -> IO ()
strategy input str p = do
  let strat = drop 3 input
  if strat == "bfs" || strat == "dfs"
    then do
      putStrLn ("Strategy set to " ++ strat ++ ".")
      loop strat p
    else do
      putStrLn "Invalid strategy."
      loop str p
