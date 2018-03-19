{- |
Module      : Language.Egison.Types
Copyright   : Akira Kawata
Licence     : MIT

This module contains static type checking algorithm for Egison
I suggest you to import this file using qualified import.
-}

module Language.Egison.TypeCheck where

import qualified Language.Egison.Types as ET
import Control.Monad.State (State,evalState,get,put)
import Control.Monad.Trans.Except (ExceptT,runExceptT)
import Data.Maybe (fromMaybe)

-- TStar is a kind of wildcard of type.
-- The argument of TFun is Tuple.
-- This is because Egison doesn't do currying, so the arity doesn't change.
-- TCollection is like a list in Haskell. All its element must have the
-- same type.
data Type = TChar | TString | TBool | TInt | TVar TVarIndex | TStar |
            TFun Type Type | TTuple [Type] | TCollection Type
            deriving (Show,Eq)
type TVarIndex = Int

-- First element of Restriction will be type valiable.
-- Second element of Restriction is what the first element refer.
type Restriction = (Type,Type) 
type Substitution = [Restriction]
type TypeEnvironmet = [(ET.EgisonExpr,Type)]
type MakeSubstition = ExceptT String (State TVarIndex)

checkTopExpr :: ET.EgisonTopExpr -> Either String (Substitution, Type)
checkTopExpr (ET.Test e) = exprToSub e
checkTopExpr _ = return ([], TStar)

exprToSub :: ET.EgisonExpr -> Either String (Substitution, Type)
exprToSub e = evalState (runExceptT $ exprToSub' [] (TVar 0) e) 1

applySub :: Substitution -> Type -> Type
applySub s (TVar i) = fromMaybe (TVar i) (lookup (TVar i) s)
applySub _ t = t

unifySub :: Substitution -> MakeSubstition Substitution
unifySub s = return s

getNewTVarIndex :: MakeSubstition TVarIndex
getNewTVarIndex = do
  i <-get
  put (i+1)
  return i

innersToExprs :: [ET.InnerExpr] -> [ET.EgisonExpr]
innersToExprs [] = []
innersToExprs (ET.ElementExpr e:rest) = e:(innersToExprs rest)
innersToExprs ((ET.SubCollectionExpr (ET.CollectionExpr is)):rest) =
    innersToExprs is ++ innersToExprs rest

exprToSub' :: TypeEnvironmet -> Type -> ET.EgisonExpr -> MakeSubstition (Substitution, Type)
exprToSub' env ty (ET.CharExpr _ ) = return ([(ty,TChar)], TChar)
exprToSub' env ty (ET.StringExpr _) = return ([(ty,TString)], TString)
exprToSub' env ty (ET.BoolExpr _) = return ([(ty,TBool)], TBool)
exprToSub' env ty (ET.IntegerExpr _) = return ([(ty,TInt)], TInt)
exprToSub' env ty (ET.IfExpr e1 e2 e3) = do
    (sub1, t1) <- exprToSub' env TBool e1
    (sub2, t2) <- exprToSub' env ty e2
    (sub3, t3) <- exprToSub' env ty e3
    sub4 <- unifySub $ (t1, TBool) : (t2, t3) : sub1 ++ sub2 ++ sub3
    return (sub4, applySub sub4 t2)
exprToSub' env ty (ET.TupleExpr es) = do
    sts <- mapM (exprToSub' env TStar) es
    let ty' = TTuple (map snd sts)
    sub <- unifySub $ (ty, ty') : (foldr (++) [] (map fst sts))
    return (sub, applySub sub ty')
exprToSub' env ty (ET.CollectionExpr es) = do
    let es' = innersToExprs es
    sts <- mapM (exprToSub' env TStar) es'
    let sub1 = foldr (++) [] (map fst sts)
    tv <- getNewTVarIndex
    let sub2 = map (\x -> (TVar tv, snd x)) sts
    let ty' = TCollection (TVar tv)
    sub3 <- unifySub $ ((ty, ty') : sub1 ++ sub2)
    return (sub3, applySub sub3 ty')
exprToSub' env ty _ = return ([], TStar)
