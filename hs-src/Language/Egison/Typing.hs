{- |
Module      : Language.Egison.Typing
Copyright   : Satoshi Egi
Licence     : MIT

This module implement type system.
-}

module Language.Egison.Typing
    (
    -- * Type System
      inferBindings
    , inferExpr
    , unify
    ) where

import Prelude hiding (mapM)

import Language.Egison.Types

inferBindings :: TypeEnv -> [Binding] -> EgisonM TypeEnv
inferBindings = undefined

inferExpr :: TypeEnv -> EgisonExpr -> EgisonM EgisonType
inferExpr = undefined

unify :: TypeEnv -> EgisonType -> EgisonType -> EgisonM EgisonType
unify = undefined
