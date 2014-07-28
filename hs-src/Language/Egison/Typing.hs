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

import Control.Arrow
import Control.Applicative
import Data.IORef
import Control.Monad.Error hiding (mapM)
import Control.Monad.State hiding (mapM, state)
import Control.Monad.Trans.Maybe

import Language.Egison.Types

inferBindings :: TypeEnv -> [Binding] -> EgisonM TypeEnv
inferBindings = undefined

inferExpr :: TypeEnv -> EgisonExpr -> EgisonM EgisonType
inferExpr env (VarExpr name) = refTypeVar env name >>= liftIO . readIORef
inferExpr env (ApplyExpr fnExpr argExpr) = do
  fnTyp <- inferExpr env fnExpr
  argTyp <- inferExpr env argExpr
  case fnTyp of
    FunctionType fnArgTyp fnRetTyp -> do
      unify env argTyp fnArgTyp
      return fnRetTyp
    _ -> undefined
  
inferExpr _ _ = undefined

unify :: TypeEnv -> EgisonType -> EgisonType -> EgisonM EgisonType
unify _ BoolType BoolType = return BoolType
unify _ _ _ = undefined
