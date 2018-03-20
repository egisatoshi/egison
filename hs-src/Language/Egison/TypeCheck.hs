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
-- TCollection is like a list in Haskell.
-- All its element must have the same type.
-- TCFun a b is a function which have arbitrary length args like (+ 1 2 3 4).
-- All TCFun arguments have same type a.
data Type = TChar | TString | TBool | TInt | TVar TVarIndex | TStar |
            TFun Type Type | TTuple [Type] | TCollection Type | TCFun Type Type | TTensor Type
            deriving (Show,Eq)
type TVarIndex = Int

-- First element of Restriction will be type valiable.
-- Second element of Restriction is what the first element refer.
type Restriction = (Type,Type) 
type Substitution = [Restriction]
-- [(Variable name, Type)]
type TypeEnvironment = [([String],Type)]
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

removeTensorMap :: ET.EgisonExpr -> ET.EgisonExpr
removeTensorMap (ET.TensorMapExpr (ET.LambdaExpr _ b) _) = removeTensorMap b
removeTensorMap (ET.TensorMap2Expr (ET.LambdaExpr _ b) _ _) = removeTensorMap b
removeTensorMap e = e

lookupTypeEnv :: [String] -> TypeEnvironment -> MakeSubstition Type
lookupTypeEnv e [] = do
  i <- getNewTVarIndex
  return $ (TVar i)
lookupTypeEnv e1 ((e2,t):r)
  | e1 == e2 = return t
  | otherwise = lookupTypeEnv e1 r

exprToSub' :: TypeEnvironment -> Type -> ET.EgisonExpr -> MakeSubstition (Substitution, Type)
exprToSub' env ty (ET.CharExpr _ ) = return ([(ty,TChar)], TChar)
exprToSub' env ty (ET.StringExpr _) = return ([(ty,TString)], TString)
exprToSub' env ty (ET.BoolExpr _) = return ([(ty,TBool)], TBool)
exprToSub' env ty (ET.IntegerExpr _) = return ([(ty,TInt)], TInt)
exprToSub' env ty (ET.VarExpr (ET.Var vn)) = do
    ty' <- lookupTypeEnv vn env
    sub <- unifySub [(ty',ty)]
    return (sub, applySub sub ty')
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
exprToSub' env ty (ET.LambdaExpr args body) = do
    let args1 = filter (/= []) $ map f args
    let body1 = removeTensorMap body
    arg1tys <- mapM (\_ -> do { x <- getNewTVarIndex; return (TVar x)}) args1
    let env1 = (zip args1 arg1tys) ++ env
    tv <- getNewTVarIndex
    (sub1,ty1) <- exprToSub' env1 (TVar tv) body1
    sub2 <- unifySub $ (ty, TFun (TTuple arg1tys) ty1):sub1
    return (sub2, applySub sub2 ty)
      where f (ET.TensorArg s) = [s]
            f _ = []
exprToSub' env ty _ = return ([], TStar)
