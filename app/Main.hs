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

handleArgs :: [String] -> IO ()
handleArgs ["help"] = putStrLn "There is no hope."
handleArgs ["repl"] = repl (([],[]), ([],[]))
handleArgs ["repl", filename] = replFile filename
handleArgs ["exec", filename] = exec filename
handleArgs args = putStrLn $ "Unknown arguments. Run `leitmotif help` for help. Args: " ++ show args

main :: IO ()
main = do
  args <- getArgs
  handleArgs args
