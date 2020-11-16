{-# LANGUAGE DeriveTraversable #-}

{- |
Module      : Language.Egison.AST
Licence     : MIT

This module defines the syntax of Egison.
-}

module Language.Egison.AST
  ( TopExpr (..)
  , ConstantExpr (..)
  , Expr (..)
  , Pattern (..)
  , VarWithIndices (..)
  , makeApply
  , Arg (..)
  , ArgPattern (..)
  , IndexExpr (..)
  , VarIndex (..)
  , PMMode (..)
  , BindingExpr (..)
  , MatchClause
  , PatternDef
  , LoopRange (..)
  , PrimitivePatPattern (..)
  , PDPatternBase (..)
  , PrimitiveDataPattern
  , Op (..)
  , Assoc (..)
  , reservedExprOp
  , reservedPatternOp
  , findOpFrom
  , stringToVarWithIndices
  ) where

import           Data.List  (find)
import           Data.Maybe (fromJust)
import           Data.Text  (Text)

-- | Top-level expression.
data TopExpr
  = Define VarWithIndices Expr -- ^ Global definition.
  | Test Expr
  | Execute Expr
    -- temporary : we will replace load to import and export
  | LoadFile String
  | Load String
  | InfixDecl Bool Op -- ^ Infix declaration. The first argument is True for pattern infix and False for expression infix.
 deriving Show

-- | Expressions for constants.
data ConstantExpr
  = CharExpr Char
  | StringExpr Text
  | BoolExpr Bool
  | IntegerExpr Integer
  | FloatExpr Double
  | SomethingExpr
  | UndefinedExpr
  deriving Show

-- | Expressions.
data Expr
  = ConstantExpr ConstantExpr
  | VarExpr String
  | FreshVarExpr
  | IndexedExpr Bool Expr [IndexExpr Expr]  -- ^ When the first argument is True, delete old index and append new one.
  | SubrefsExpr Bool Expr Expr
  | SuprefsExpr Bool Expr Expr
  | UserrefsExpr Bool Expr Expr
  | TupleExpr [Expr]
  | CollectionExpr [Expr]
  | ConsExpr Expr Expr
  | JoinExpr Expr Expr
  | HashExpr [(Expr, Expr)]
  | VectorExpr [Expr]

  | LambdaExpr [Arg ArgPattern] Expr
  | LambdaExpr' [Arg String] Expr -- ^ Only appears during the desugar.
  | MemoizedLambdaExpr [String] Expr
  | CambdaExpr String Expr
  | PatternFunctionExpr [String] Pattern

  | IfExpr Expr Expr Expr
  | LetExpr [BindingExpr] Expr
  | LetRecExpr [BindingExpr] Expr
  | WithSymbolsExpr [String] Expr

  | MatchExpr PMMode Expr Expr [MatchClause]
  | MatchAllExpr PMMode Expr Expr [MatchClause]
  | MatchLambdaExpr Expr [MatchClause]
  | MatchAllLambdaExpr Expr [MatchClause]

  | MatcherExpr [PatternDef]
  | AlgebraicDataMatcherExpr [(String, [Expr])]

  | QuoteExpr Expr
  | QuoteSymbolExpr Expr
  | WedgeApplyExpr Expr [Expr]

  | DoExpr [BindingExpr] Expr

  | PrefixExpr String Expr
  | InfixExpr Op Expr Expr
  -- ^ Nothing on the lhs or rhs denotes the missing argument.
  --   For example, @(+ 1)@ is represented like "SectionExpr + Nothing 1".
  --   There cannot be "SectionExpr op (Just _) (Just _)"
  | SectionExpr Op (Maybe Expr) (Maybe Expr)

  | SeqExpr Expr Expr
  | ApplyExpr Expr [Expr]
  | CApplyExpr Expr Expr
  | AnonParamFuncExpr Integer Expr      -- ^ Example: @2#(%1 + %2)@
  | AnonTupleParamFuncExpr Integer Expr -- ^ Example: @(2)#(%1 + %2)@
  | AnonListParamFuncExpr Integer Expr  -- ^ Example: @[2]#(%1 + %2)@
  | AnonParamExpr Integer

  | GenerateTensorExpr Expr Expr
  | TensorExpr Expr Expr
  | TensorContractExpr Expr
  | TensorMapExpr Expr Expr
  | TensorMap2Expr Expr Expr Expr
  | TransposeExpr Expr Expr
  | FlipIndicesExpr Expr          -- ^ This doesn't appear in user program

  | FunctionExpr [String]
  deriving Show

data VarWithIndices = VarWithIndices String [VarIndex]
  deriving Show

data Arg a
  = ScalarArg a
  | InvertedScalarArg a
  | TensorArg a
  deriving Show

-- | Available patterns at the argument position of functions.
data ArgPattern
  = APWildCard
  | APPatVar String
  | APInductivePat String [Arg ArgPattern]
  | APTuplePat [Arg ArgPattern]
  | APEmptyPat
  | APConsPat (Arg ArgPattern) ArgPattern
  | APSnocPat ArgPattern (Arg ArgPattern)
  deriving Show

data VarIndex
  = VSubscript String
  | VSuperscript String
  | VGroupScripts [VarIndex]
  | VSymmScripts [VarIndex]
  | VAntiSymmScripts [VarIndex]
  deriving Show

data IndexExpr a
  = Subscript a          -- ^ Example: @x_i@
  | Superscript a        -- ^ Example: @x~i@
  | SupSubscript a       -- ^ Example: @x~_i@
  | MultiSubscript a a   -- ^ Example: @x_(a_1)..._(a_n)@
  | MultiSuperscript a a -- ^ Example: @x~(a_1)...~(a_n)@
  | Userscript a         -- ^ Example: @f|x@
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Mode of pattern matching (BFS or DFS).
data PMMode = BFSMode | DFSMode
  deriving Show

data BindingExpr
  = Bind PrimitiveDataPattern Expr
  | BindWithIndices VarWithIndices Expr
  deriving Show

type MatchClause = (Pattern, Expr)
type PatternDef  = (PrimitivePatPattern, Expr, [(PrimitiveDataPattern, Expr)])

data Pattern
  = WildCard
  | PatVar String
  | ValuePat Expr
  | PredPat Expr
  | IndexedPat Pattern [Expr]
  | LetPat [BindingExpr] Pattern
  | InfixPat Op Pattern Pattern -- ^ This includes AndPat,OrPat,InductivePat(cons\/join)
  | NotPat Pattern
  | AndPat Pattern Pattern
  | OrPat Pattern Pattern
  | ForallPat Pattern Pattern
  | TuplePat [Pattern]
  | InductivePat String [Pattern]
  | LoopPat String LoopRange Pattern Pattern
  | ContPat
  | PApplyPat Expr [Pattern]
  | VarPat String
  | InductiveOrPApplyPat String [Pattern]
  | SeqNilPat
  | SeqConsPat Pattern Pattern
  | LaterPatVar
  -- For symbolic computing
  | DApplyPat Pattern [Pattern]
  deriving Show

data LoopRange = LoopRange Expr Expr Pattern
  deriving Show

data PrimitivePatPattern
  = PPWildCard
  | PPPatVar
  | PPValuePat String
  | PPInductivePat String [PrimitivePatPattern]
  | PPTuplePat [PrimitivePatPattern]
  deriving Show

data PDPatternBase var
  = PDWildCard
  | PDPatVar var
  | PDInductivePat String [PDPatternBase var]
  | PDTuplePat [PDPatternBase var]
  | PDEmptyPat
  | PDConsPat (PDPatternBase var) (PDPatternBase var)
  | PDSnocPat (PDPatternBase var) (PDPatternBase var)
  | PDConstantPat ConstantExpr
  deriving (Functor, Foldable, Show)

type PrimitiveDataPattern = PDPatternBase String

-- | Infix or prefix operators.
data Op
  = Op { repr     :: String -- ^ Syntactic representation of the operator.
       , priority :: Int
       , assoc    :: Assoc
       , isWedge  :: Bool   -- ^ This is True iff operator is prefixed with '!'. Only used for expression infix.
       }
  deriving (Eq, Ord, Show)

data Assoc
  = InfixL
  | InfixR
  | InfixN
  | Prefix
  deriving (Eq, Ord)

instance Show Assoc where
  show InfixL = "infixl"
  show InfixR = "infixr"
  show InfixN = "infix"
  show Prefix = "prefix"

-- | Definitions of built-in operators for expressions.
reservedExprOp :: [Op]
reservedExprOp =
  [ Op "!"  8 Prefix False -- Wedge
  , Op "-"  7 Prefix False -- Negate
  , Op "%"  7 InfixL False -- primitive function
  , Op "++" 5 InfixR False
  , Op "::" 5 InfixR False
  , Op "="  4 InfixL False -- primitive function
  , Op "<=" 4 InfixL False -- primitive function
  , Op ">=" 4 InfixL False -- primitive function
  , Op "<"  4 InfixL False -- primitive function
  , Op ">"  4 InfixL False -- primitive function
  ]

-- | Definitions of built-in operators for patterns.
reservedPatternOp :: [Op]
reservedPatternOp =
  [ Op "::" 5 InfixR False -- cons (desugared)
  , Op "++" 5 InfixR False -- join (desugared)
  , Op "&"  3 InfixR False
  , Op "|"  2 InfixR False
  ]

-- | Find the instance of Op from the representation.
findOpFrom :: String -> [Op] -> Op
findOpFrom op table = fromJust $ find ((== op) . repr) table

stringToVarWithIndices :: String -> VarWithIndices
stringToVarWithIndices name = VarWithIndices name []

-- | Make @ApplyExpr@ from a function name in string and a list of arguments.
makeApply :: String -> [Expr] -> Expr
makeApply func args = ApplyExpr (VarExpr func) args
