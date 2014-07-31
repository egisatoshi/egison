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
inferExpr env (LambdaExpr names expr) = do
  argTyps <- undefined
  bodyTyp <- inferExpr (extendTypeEnv env (zip names argTyps)) expr
  return $ FunctionType argTyps bodyTyp
inferExpr env (ApplyExpr fnExpr argExpr) = do
  fnTyp <- inferExpr env fnExpr
  argTyp <- inferExpr env argExpr
  fnTyp' <- unify env (FunctionType argTyp WildCardType) fnTyp
  case fnTyp' of
    FunctionType _ retTyp -> do
      return retTyp
    _ -> undefined
  
inferExpr _ _ = undefined

unify :: TypeEnv -> EgisonType -> EgisonType -> EgisonM EgisonType
unify _ WildCardType WildCardType = return WildCardType
unify _ BoolType BoolType = return BoolType
unify _ _ _ = undefined
