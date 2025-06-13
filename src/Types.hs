module Types where

{-
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
-}

import Text.Trifecta.Result (ErrInfo)

-- Identifiers
type TConIdentifier = String
type VConIdentifier = String
type ValueIdentifier = String

-- Bindings
type TypeBinding  = (ValueIdentifier, Type)
type ValueBinding = (ValueIdentifier, Value)

-- I/O Primitives
data IOPrimOp
  = GetLine
  | Print
  | ReadFile
  | IOSeq
  deriving (Eq, Show)

-- Core data types
data Type
  = TInt | TChar | TStr
  | Fn [Type] Type
  | TCon TConIdentifier
  | PrimIOTy Type | TUnit
  deriving (Eq, Show)

retType :: Type -> Type
retType (Fn _ r) = r
retType t = t

data Pattern
  = PVar ValueIdentifier
  | PCon VConIdentifier [Pattern]
  | PIntLit Integer
  | PCharLit Char
  | PStrLit String
  | PWildcard
  deriving (Eq, Show)

data Op
  = Plus
  | Mult
  | Minus
  | StrLen
  | StrHead
  | StrTail
  | IOP IOPrimOp
  deriving (Eq, Show)

data Expr
  = Var ValueIdentifier
  | Abs [TypeBinding] Expr
  | App Expr [Expr]
  | LInt Integer | LChar Char | LStr String
  | Prim Op
  | LetData TConIdentifier [(VConIdentifier, Type)] Expr
  | EVCon VConIdentifier
  | Match Expr [(Pattern, Expr)]
  deriving (Eq, Show)

data Decl
  = ADTDecl TConIdentifier [(VConIdentifier, Type)]
  | FnDecl ValueIdentifier Type [TypeBinding] Expr
  deriving (Eq, Show)

type Program = [Decl]

data VIOPrim
  = VGetLine
  | VPrint String
  | VReadFile String
  | VIOSeq VIOPrim VIOPrim
  deriving (Eq, Show)

data Value
  = VInt Integer | VChar Char | VStr String
  | VLamClosure [ValueIdentifier] Expr EvalContext
  | VRecClosure ValueIdentifier [ValueIdentifier] Expr EvalContext
  | VCon VConIdentifier
  | VData VConIdentifier [Value]
  | VIOP VIOPrim
  deriving (Eq)

instance Show Value where
  show (VInt n)  = show n
  show (VChar c) = show c
  show (VStr s) = show s
  show (VLamClosure args body env) = "lambda{"++show args++". "++show body++" -| "++show env++"}"
  show (VRecClosure name args body env) = "rec["++name++"]{"++show args++". "++show body++" -| "++show env++"}"
  show (VCon name) = name
  show (VData name args) = name ++ " " ++ show args
  show (VIOP viop) = show viop

-- Contexts
type VConArity = (VConIdentifier, Int)
type EvalContext = ([ValueBinding], [VConArity])
type TypeContext = [TypeBinding]
type KindContext = [TConIdentifier]  -- In a polymorphic language, this maps to kinds. We're monomorphic so all kinds are *
type Context = (TypeContext, KindContext)

-- Errors
data ParseError
  = EOF String
  | Trifecta ErrInfo
  | UnexpectedClosingBracket
  deriving (Show)

data TypeCheckError
  = AnnotateWithName String TypeCheckError

  | UnknownVarName ValueIdentifier
  | TypeMismatch Type Type

  | NonFunctionAppHead Type
  | PrimOpTypeMismatch Op Type Type
  | PrimOpAppMismatch Op Type

  | DuplicateTConDef TConIdentifier
  | DuplicateVConDef VConIdentifier
  | IllTypedVCon VConIdentifier

  | UnknownVConName VConIdentifier

  | IllTypedFnDecl ValueIdentifier

  -- These should not be reachable (since they would produce parse errors first)
  | EmptyCaseClauseList

  | UNPORTEDERROR String
  deriving (Eq, Show)

data EvalError
  = NonExhaustivePatternMatch Value
  | MatchingOnNonVCon Value

  -- Only for evalProgram
  | NoMain

  -- These should not be reachable
  | Can'tApplyNonClosureVConOrPrim Value
  | ScopeError ValueIdentifier
  | UnappliedPrimOp Op
  | TypeError
  deriving (Eq, Show)

data Error
  = PE  [ParseError]
  | TCE [TypeCheckError]
  | EvE [EvalError]
  deriving (Show)
