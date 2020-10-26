{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : Language.Egison.Math
Licence     : MIT

This module provides the interface of Egison's computer algebra system.
-}

module Language.Egison.Math
  ( ScalarData (..)
  , PolyExpr (..)
  , TermExpr (..)
  , Monomial
  , SymbolExpr (..)
  , Printable (..)
  , pattern ZeroExpr
  , pattern SingleSymbol
  , pattern SingleTerm
  , mathNormalize'
  , rewriteSymbol
  , mathPlus
  , mathMult
  , mathDiv
  , mathNumerator
  , mathDenominator
  , mathNegate
  ) where

import           Language.Egison.Math.Arith
import           Language.Egison.Math.Expr
import           Language.Egison.Math.Normalize
import           Language.Egison.Math.Rewrite
