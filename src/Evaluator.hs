module Evaluator where

import Types

import Data.Bifunctor (second)

pair :: a -> b -> (a, b)
pair a b = (a, b)

arity :: Type -> Int
arity (Fn as _) = length as
arity _ = 0

match :: Value -> Pattern -> Maybe [ValueBinding]
match (VInt n) (PIntLit n')
  | n == n'   = Just []
  | otherwise = Nothing
match (VChar c) (PCharLit c')
  | c == c'   = Just []
  | otherwise = Nothing
match (VStr s) (PStrLit s')
  | s == s'   = Just []
  | otherwise = Nothing
match (VBool b) (PBoolLit b')
  | b == b'   = Just []
  | otherwise = Nothing

match (VData name []) (PCon name' [])
  | name == name' = Just []
  | otherwise     = Nothing

match (VData name vs) (PCon name' ps)
  | name == name' = foldMap id $ zipWith match vs ps
  | otherwise     = Nothing

match v (PVar x)   = Just [(x, v)]
match _ PWildcard  = Just []

match _ _ = Nothing


eval :: EvalContext -> Expr -> Either EvalError Value
eval (vs,_) (Var x) = case lookup x vs of
  Nothing -> Left $ ScopeError x
  Just (VRecClosure _ [] b env) -> eval env b
  Just v  -> Right v

eval env (Abs args b) = Right $ VLamClosure (fst <$> args) b env

eval env (App (Prim op) xs) = do
  xs' <- traverse (eval env) xs
  case (op, xs') of
    (Plus,  [VInt x, VInt y]) -> Right $ VInt $ x + y
    (Mult,  [VInt x, VInt y]) -> Right $ VInt $ x * y
    (Minus, [VInt x, VInt y]) -> Right $ VInt $ x - y
    (NumEq, [VInt x, VInt y]) -> Right $ VBool $ x == y

    (StrLen,  [VStr s]) -> Right $ VInt $ toInteger $ length s
    (StrHead, [VStr s])  -> Right $ VChar $ head s
    (StrTail, [VStr ""]) -> Right $ VStr  $ ""
    (StrTail, [VStr s])  -> Right $ VStr  $ tail s
    (StrEq,   [VStr s1, VStr s2]) -> Right $ VBool $ s1 == s2

    (IOP Print, [VStr s]) -> Right $ VIOP $ VPrint s
    (IOP ReadFile, [VStr s]) -> Right $ VIOP $ VReadFile s
    (IOP IOSeq, [VIOP l, VIOP r]) -> do
      Right $ VIOP $ VIOSeq l r
    --_ -> Left TypeError

eval env (App f xs) = do
  xs' <- traverse (eval env) xs
  case eval env f of
    Right (VLamClosure names body (vs', as')) ->
      eval (zip names xs' ++ vs', as') body

    Right v@(VRecClosure name names body (vs', as')) ->
      eval ((name, v) : zip names xs' ++ vs', as') body

    Right (VCon name) -> Right $ VData name xs'
    Right v -> Left $ Can'tApplyNonClosureVConOrPrim v
    Left err -> Left err

eval _ (LInt  n) = Right $ VInt n
eval _ (LChar c) = Right $ VChar c
eval _ (LStr  s) = Right $ VStr s
eval _ (LBool b) = Right $ VBool b

eval _ (Prim (IOP GetLine)) = Right $ VIOP VGetLine

eval _ (Prim op) = Left $ UnappliedPrimOp op

eval (vs, _) (LetData _ vts e) =
  let arities' = map (second arity) vts
      env' = (vs, arities')
  in eval env' e

eval (_, arities) (EVCon name) = case lookup name arities of
    Just 0 -> Right $ VData name []
    Just _ -> Right $ VCon  name
    Nothing -> Left $ ScopeError name

eval env@(vs, as) (Match scrutinee patterns) = do
  value <- eval env scrutinee
  let go :: Value -> [(Pattern, Expr)] -> Either EvalError Value
      go v [] = Left $ NonExhaustivePatternMatch v
      go v ((p, b) : ps) = case match v p of
        Just bindings -> eval (vs ++ bindings, as) b
        Nothing -> go v ps
  go value patterns

evalDecl :: EvalContext -> Decl -> Either EvalError EvalContext
evalDecl (vs, as) (ADTDecl _ vts) = Right (vs, map (second arity) vts ++ as)
evalDecl env@(vs, as) (FnDecl name _ bindings body) = Right ((name, VRecClosure name (map fst bindings) body env) : vs, as)


evalProgram :: EvalContext -> Program -> Either EvalError EvalContext
evalProgram env [] = Right env
evalProgram env (decl:decls) = do
  env' <- evalDecl env decl
  evalProgram env' decls

exec :: Value -> IO Value
exec (VIOP (VGetLine)) = VStr <$> getLine
exec (VIOP (VPrint s)) = putStr s >> pure (VStr s)
exec (VIOP (VReadFile s)) = VStr <$> readFile s
exec (VIOP (VIOSeq l r)) = exec (VIOP l) >> exec (VIOP r)
exec v = pure v

runProgram :: Program -> Either EvalError Value
runProgram p = do
  (vs, _) <- evalProgram ([],[]) p
  case lookup "main" vs of
    Just (VRecClosure _ [] b env) -> eval env b
    Just v  -> Right v
    Nothing -> Left NoMain
