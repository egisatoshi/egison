{- |
Module      : Language.Egison.Types
Copyright   : Akira Kawata
Licence     : MIT

This module contains static type checking algorithm for Egison
I suggest you to import this file using qualified import.
-}

module Language.Egison.TypeCheck where

import qualified Language.Egison.Types as ET
import Control.Monad.State (State,evalState)
import Control.Monad.Trans.Maybe (MaybeT,runMaybeT)

data Type = TInt | TString | TVar TVarIndex | TStar deriving (Show,Eq)
type TVarIndex = Int
type Restriction = (Type,Type)
type Substitution = [Restriction]
type TypeEnvironmet = [(ET.EgisonExpr,Type)]
type MakeSubstition = MaybeT (State TVarIndex)

checkTopExpr :: ET.EgisonTopExpr -> Maybe (Substitution, Type)
checkTopExpr (ET.Test e) = exprToSub e
checkTopExpr _ = return ([], TStar)

exprToSub :: ET.EgisonExpr -> Maybe (Substitution, Type)
exprToSub e = evalState (runMaybeT $ exprToSub' [] (TVar 0) e) 1

exprToSub' :: TypeEnvironmet -> Type -> ET.EgisonExpr -> MakeSubstition (Substitution, Type)
exprToSub' env ty (ET.IntegerExpr _) = return ([], TInt)
exprToSub' env ty (ET.StringExpr _) = return ([], TString)
exprToSub' env ty _ = return ([], TStar)
