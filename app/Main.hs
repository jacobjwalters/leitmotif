module Main where

import System.Environment (getArgs)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, hFlush)

import Text.Trifecta.Result (ErrInfo(_errDoc))

import Leitmotif (evaluateProgram, evaluateExpr, executeProgram)
import Types (Context, EvalContext, Error(..), ParseError(Trifecta))

repl :: (Context, EvalContext) -> IO ()
repl (ctx, env) = do
  hSetBuffering stdout NoBuffering
  putStr "leitmotif> "
  str <- getLine
  case evaluateExpr ctx env str of
    Left  err -> print err >> repl (ctx, env)
    Right v   -> print v   >> repl (ctx, env)

replFile :: String -> IO ()
replFile filename = do
  str <- readFile filename
  case evaluateProgram str of
    Left (PE [Trifecta err]) -> print $ _errDoc err
    Left err      -> print err
    Right env -> putStrLn ("Loaded " ++ filename) >> repl env

exec :: String -> IO ()
exec filename = do
  str <- readFile filename
  case executeProgram str of
    Left err -> print err
    Right v -> print v

helpString :: String
helpString = unlines
  [ "Leitmotif v0.1.0.0"
  , "Usage:"
  , "leitmotif help                Display this message"
  , "leitmotif repl                Enter an empty REPL session"
  , "leitmotif repl <filename>     Enter an REPL session, loading the definitions from <filename>"
  , "leitmotif exec <filename>     Load the definitions from <filename>, and then run `main`"
  ]

handleArgs :: [String] -> IO ()
handleArgs [] = putStrLn helpString
handleArgs ["help"] = putStrLn helpString
handleArgs ["repl"] = repl (([],[]), ([],[]))
handleArgs ["repl", filename] = replFile filename
handleArgs ["exec", filename] = exec filename
handleArgs args = putStrLn $ "Unknown arguments. Args: " ++ show args ++ "\n" ++ helpString

main :: IO ()
main = do
  args <- getArgs
  handleArgs args
