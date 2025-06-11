module Corvid
  ( module P
  , module TypeChecking
  , parseExpr, parseProgram
  , typecheckExpr, typecheckProgram
  , evaluateExpr, evaluateProgram
  , executeProgram
  , tc, ev
  ) where

import Types
import qualified Parser as P
import TypeChecking
import Evaluator

import Text.Trifecta.Result (Result(Failure, Success))

parseExpr :: String -> Either Error Expr
parseExpr input = case P.parseExpr input of
  Failure err -> Left $ PE [Trifecta err]
  Success e -> Right e

parseProgram :: String -> Either Error Program
parseProgram input = case P.parseProgram input of
  Failure err -> Left $ PE [Trifecta err]
  Success e -> Right e

typecheckExpr :: Context -> String -> Either Error Type
typecheckExpr ctx input = case parseExpr input of
  Left err -> Left err
  Right e -> case synth ctx e of
    Left errs -> Left $ TCE errs
    Right ty -> Right ty

typecheckProgram :: String -> Either Error Context
typecheckProgram input = case parseProgram input of
  Left err -> Left err
  Right e -> case synthProgram ([],[]) e of
    Left errs -> Left $ TCE errs
    Right ctx -> Right ctx

evaluateExpr :: Context -> EvalContext -> String -> Either Error Value
evaluateExpr ctx env input = do
  e <- parseExpr input
  _ <- typecheckExpr ctx input
  case eval env e of
    Left err -> Left $ EvE [err]
    Right v -> Right v

evaluateProgram :: String -> Either Error (Context, EvalContext)
evaluateProgram input = do
  p <- parseProgram input
  gamma <- typecheckProgram input
  case evalProgram ([],[]) p of
    Left err -> Left $ EvE [err]
    Right env -> Right (gamma, env)

executeProgram :: String -> Either Error Value
executeProgram input = do
  p <- parseProgram input
  _ <- typecheckProgram input
  case runProgram p of
    Left err -> Left $ EvE [err]
    Right v -> Right v


-- shorthands for loading the module in repl
tc :: String -> Either Error Type
tc = typecheckExpr ([],[])

ev :: String -> Either Error Value
ev = evaluateExpr ([],[]) ([],[])

testProg = unlines
  [ "foo : Int = 3"
  , ""
  , "bar : Int -> Int = lambda x : Int. x + 1"
  , ""
  , "main : Int = bar foo"
  ]
