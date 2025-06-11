module Main where

import System.Environment (getArgs)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout, hFlush)

import Corvid (evaluateProgram, evaluateExpr, executeProgram)
import Types (Context, EvalContext)

repl :: (Context, EvalContext) -> IO ()
repl (ctx, env) = do
  hSetBuffering stdout NoBuffering
  putStr "corvid> "
  str <- getLine
  case evaluateExpr ctx env str of
    Left  err -> print err >> repl (ctx, env)
    Right v   -> print v   >> repl (ctx, env)

replFile :: String -> IO ()
replFile filename = do
  str <- readFile filename
  case evaluateProgram str of
    Left err  -> print err
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
handleArgs args = putStrLn $ "Unknown arguments. Run `corvid help` for help. Args: " ++ show args

main :: IO ()
main = do
  args <- getArgs
  handleArgs args
