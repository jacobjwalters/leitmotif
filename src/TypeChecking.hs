module TypeChecking where

import Types

retTy :: Type -> Type
retTy (Fn _ t) = retTy t
retTy t = t

fnToList :: Type -> [Type]
fnToList (Fn a b) = a ++ [b]
fnToList t = [t]

combinePatternChecks :: [Either [TypeCheckError] TypeContext] -> Either [TypeCheckError] TypeContext
combinePatternChecks [] = Left [UNPORTEDERROR "Called combinePatternChecks on empty list"]
combinePatternChecks (pc : []) = pc
combinePatternChecks (Left errs : cs) = case combinePatternChecks cs of
  Left  errs' -> Left $ errs ++ errs'
  Right _     -> Left $ errs
combinePatternChecks (Right ctx : cs) = case combinePatternChecks cs of
  Left  errs -> Left errs
  Right ctx' -> Right $ ctx ++ ctx'

checkPattern :: Context -> Type -> Pattern -> Either [TypeCheckError] TypeContext
checkPattern _ TInt  (PIntLit  _) = Right []
checkPattern _ TChar (PCharLit _) = Right []
checkPattern _ ty (PIntLit  _) = Left [TypeMismatch TInt  ty]
checkPattern _ ty (PCharLit _) = Left [TypeMismatch TChar ty]

checkPattern _ ty (PVar x)  = Right [(x, ty)]
checkPattern _ _  PWildcard = Right []

checkPattern (gamma, _) ty (PCon vcon []) = case lookup vcon gamma of
  Just vconTy -> if ty == vconTy
                 then Right []
                 else Left [AnnotateWithName vcon $ TypeMismatch ty vconTy]
  Nothing -> Left [UnknownVConName vcon]

checkPattern (gamma, delta) ty (PCon vcon args) = do
  vconTy <- case lookup vcon gamma of
    Just t -> pure t
    Nothing -> Left [UnknownVConName vcon]

  let expectedSubpatternTys = fnToList vconTy
  let subpatternTys = map (\(ety, arg) -> checkPattern (gamma, delta) ety arg) $ zip expectedSubpatternTys args

  if ty == retTy vconTy
     then if length args + 1 == length expectedSubpatternTys
          then combinePatternChecks subpatternTys
          else Left [UNPORTEDERROR $ "Pattern constructor {vcon} was given the wrong number of arguments:" ++ show vcon]
     else Left [TypeMismatch ty vconTy]

synth :: Context -> Expr -> Either [TypeCheckError] Type
synth (gamma, _) (Var x) = case lookup x gamma of
  Just t -> Right t
  Nothing -> Left [UnknownVarName x]

synth (gamma, delta) (Abs vars body) = do
  ret <- synth (vars ++ gamma, delta) body
  pure $ Fn (map snd vars) ret

synth ctx (App f xs) = case (synth ctx f, traverse (synth ctx) xs) of
  (Left err,  Left err') -> Left $ err ++ err'
  (Left err,  Right _) -> Left err
  (Right _, Left err) -> Left err
  (Right (Fn args r), Right xts) ->
    if xts == args
    then Right r
    else Left [UNPORTEDERROR "Function argument mismatch in application"]
  (Right ft, Right _) -> Left [NonFunctionAppHead ft]

synth _ (LInt _) = Right TInt
synth _ (LChar _) = Right TChar

synth _ (Prim Plus)  = Right $ Fn [TInt, TInt] TInt
synth _ (Prim Mult)  = Right $ Fn [TInt, TInt] TInt
synth _ (Prim Minus) = Right $ Fn [TInt, TInt] TInt

synth (gamma, delta) (LetData tcon vcons e) =
  let newTcon = not $ tcon `elem` delta
      newVcons = all (\(vcon,_) -> not $ vcon `elem` (fst <$> gamma)) vcons
      compatibleVcons = all (\(_,t) -> retTy t == TCon tcon) vcons
      gamma' = gamma ++ vcons
      delta' = tcon : delta
  in if newTcon
     then if newVcons
          then if compatibleVcons
               then synth (gamma', delta') e
               else Left [IllTypedVCon $ show tcon]  -- TODO: we should show which vcon is wrong
          else Left [DuplicateVConDef $ show tcon]  -- TODO: !!
     else Left [DuplicateTConDef $ show tcon]

synth (gamma, _) (EVCon name) = case lookup name gamma of
  Just t -> Right t
  Nothing -> Left [UnknownVConName name]

synth ctx (Match scrutinee ps) = do
  scrutineeTy <- synth ctx scrutinee
  combineCaseClauses $ map (synthCaseClause ctx scrutineeTy) ps
  where
    synthCaseClause :: Context -> Type -> (Pattern, Expr) -> Either [TypeCheckError] Type
    synthCaseClause (gamma, delta) sty (p, e) = do
      gamma' <- checkPattern (gamma, delta) sty p
      synth (gamma' ++ gamma, delta) e

    combineCaseClauses :: [Either [TypeCheckError] Type] -> Either [TypeCheckError] Type
    combineCaseClauses [] = Left [EmptyCaseClauseList]
    combineCaseClauses (cc : []) = cc
    combineCaseClauses (Left err : _) = Left err
    combineCaseClauses (Right ty : ccs) = case combineCaseClauses ccs of
      Right _ -> Right ty
      err -> err

synthDecl :: Context -> Decl -> Either [TypeCheckError] Context
synthDecl (gamma, delta) (ADTDecl tcon vcons) =
  let newTcon = not $ tcon `elem` delta
      newVcons = all (\(vcon,_) -> not $ vcon `elem` (fst <$> gamma)) vcons
      compatibleVcons = all (\(_,t) -> retTy t == TCon tcon) vcons
      gamma' = gamma ++ vcons
      delta' = tcon : delta
  in if newTcon
     then if newVcons
          then if compatibleVcons
               then Right (gamma', delta')
               else Left [IllTypedVCon $ show tcon]  -- TODO: we should show which vcon is wrong
          else Left [DuplicateVConDef $ show tcon]  -- TODO: !!
     else Left [DuplicateTConDef $ show tcon]


synthDecl (gamma, delta) (FnDecl name ty bindings body) = do
  let gamma' = (name, ty) : bindings ++ gamma
  ty' <- synth (gamma', delta) body
  if retType ty == ty'
    then Right (gamma', delta)
    else Left [IllTypedFnDecl name]

synthProgram :: Context -> Program -> Either [TypeCheckError] Context
synthProgram ctx [] = Right ctx
synthProgram ctx (decl:decls) = do
  ctx' <- synthDecl ctx decl
  synthProgram ctx' decls
