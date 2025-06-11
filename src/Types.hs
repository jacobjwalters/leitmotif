module Types where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict

-- Identifiers
type TConIdentifier = String
type VConIdentifier = String
type ValueIdentifier = String

-- Bindings
type TypeBinding  = (ValueIdentifier, Type)
type ValueBinding = (ValueIdentifier, Value)

-- Core data types
data Type
  = TInt | TChar
  | Fn [Type] Type
  | TCon TConIdentifier
  deriving (Eq, Show)

retType :: Type -> Type
retType (Fn _ r) = r
retType t = t

data Pattern
  = PVar ValueIdentifier
  | PCon VConIdentifier [Pattern]
  | PIntLit Integer
  | PCharLit Char
  | PWildcard
  deriving (Eq, Show)

data Op
  = Plus
  | Mult
  | Minus
  deriving (Eq, Show)

data Expr
  = Var ValueIdentifier
  | Abs [TypeBinding] Expr
  | App Expr [Expr]
  | LInt Integer | LChar Char
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

data Value
  = VInt Integer | VChar Char
  | VLamClosure [ValueIdentifier] Expr EvalContext
  | VRecClosure ValueIdentifier [ValueIdentifier] Expr EvalContext
  | VCon VConIdentifier
  | VData VConIdentifier [Value]
  deriving (Eq, Show)

-- Contexts
type VConArity = (VConIdentifier, Int)
type EvalContext = ([ValueBinding], [VConArity])
type TypeContext = [TypeBinding]
type KindContext = [TConIdentifier]  -- In a polymorphic language, this maps to kinds. We're monomorphic so all kinds are *
type Context = (TypeContext, KindContext)

-- Errors
data ParseError
  = EOF String
  | UnexpectedClosingBracket
  deriving (Eq, Show)

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
  deriving (Eq, Show)
