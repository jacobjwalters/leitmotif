module Types where

{-
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
-}

import Data.List (intercalate)

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
  = TInt | TChar | TStr | TBool
  | Fn [Type] Type
  | TCon TConIdentifier
  | PrimIOTy Type | TUnit
  deriving (Eq)

instance Show Type where
  show TInt = "Int"
  show TChar = "Char"
  show TStr = "Str"
  show TBool = "Bool"
  show (Fn [] r) = show r
  show (Fn as r) = intercalate " -> " (map show as) ++ " -> " ++ show r
  show (TCon name) = name
  show (PrimIOTy ty) = "IO " ++ show ty
  show TUnit = "()"

retType :: Type -> Type
retType (Fn _ r) = r
retType t = t

data Pattern
  = PVar ValueIdentifier
  | PCon VConIdentifier [Pattern]
  | PIntLit Integer
  | PCharLit Char
  | PStrLit String
  | PBoolLit Bool
  | PWildcard
  deriving (Eq, Show)

data Op
  = Plus
  | Mult
  | Minus
  | NumEq
  | StrLen
  | StrHead
  | StrTail
  | StrEq
  | IOP IOPrimOp
  deriving (Eq, Show)

data Expr
  = Var ValueIdentifier
  | Abs [TypeBinding] Expr
  | App Expr [Expr]
  | LInt Integer | LChar Char | LStr String | LBool Bool
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
  = VInt Integer | VChar Char | VStr String | VBool Bool
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
  show (VBool b) = show b
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

  | NonFunctionAppHead Type [Type]

  | DuplicateTConDef TConIdentifier
  | DuplicateVConDef VConIdentifier
  | IllTypedVCon VConIdentifier

  | UnknownVConName VConIdentifier

  | IllTypedFnDecl ValueIdentifier Type Type

  -- These should not be reachable (since they would produce parse errors first)
  | EmptyCaseClauseList

  | UNPORTEDERROR String
  deriving (Eq)

instance Show TypeCheckError where
  show (AnnotateWithName name err) = name ++ ": " ++ show err
  show (UnknownVarName name) = "Unknown variable name: " ++ name
  show (TypeMismatch ex got) = "Type mismatch: expected " ++ show ex ++ ", got " ++ show got
  show (NonFunctionAppHead ty tys) = "Non-function type in head position for App: " ++ show ty ++ ", supplied with " ++ show tys
  show (DuplicateTConDef name) = "Type constructor already defined: " ++ name
  show (DuplicateVConDef name) = "Value constructor already defined: in " ++ name
  show (IllTypedVCon name) = "Ill typed value constructor: in " ++ name
  show (UnknownVConName name) = "Unknown value constructor: in " ++ name
  show (IllTypedFnDecl name ex got) = "Ill typed function declaration: " ++ name ++ " should have type " ++ show ex ++ ", but actually has type " ++ show got
  show (EmptyCaseClauseList) = "Empty match clause list: This indicates a parser bug"
  show (UNPORTEDERROR err) = err

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
