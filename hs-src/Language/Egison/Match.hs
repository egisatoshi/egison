{- |
Module      : Language.Egison.Match
Licence     : MIT

This module defines some data types for pattern matching.
-}

module Language.Egison.Match
    ( Match
    , MatchingTree (..)
    , MatchingState (..)
    , PatternBinding
    , LoopPatContext (..)
    , SeqPatContext (..)
    , nullMState
    , MatchM
    , matchFail
    ) where

import           Control.Monad.Trans.Maybe

import           Language.Egison.Data
import           Language.Egison.IExpr

--
-- Pattern Matching
--

-- | Pattern matching results are expressed as bindings.
type Match = [Binding]

data MatchingState = MState
  { -- | Environment (needed for closed-scope pattern function).
    mStateEnv      :: Env
    -- | Context for loop patterns.
  , loopPatCtx     :: [LoopPatContext]
    -- | Context for sequential patterns.
  , seqPatCtx      :: [SeqPatContext]
    -- | Pattern matching results.
  , mStateBindings :: [Binding]
    -- | Matching tree.
  , mTrees         :: [MatchingTree]
  }

instance Show MatchingState where
  show ms = "(MState " ++ unwords ["_", "_", "_", show (mStateBindings ms), show (mTrees ms)] ++ ")"

data MatchingTree
  = MAtom IPattern WHNFData Matcher
  | MNode [PatternBinding] MatchingState
  deriving Show

type PatternBinding = (String, IPattern)

data LoopPatContext =
  LoopPatContext
    (String, ObjectRef) -- ^ (iterator name, iterator value)
    ObjectRef           -- ^ End values of the index
    IPattern            -- ^ End pattern
    IPattern            -- ^ Repeat pattern
    IPattern            -- ^ Final pattern

data SeqPatContext
  = SeqPatContext [MatchingTree] IPattern [Matcher] [WHNFData]
  | ForallPatContext [Matcher] [WHNFData]

nullMState :: MatchingState -> Bool
nullMState MState{ mTrees = [] }                = True
nullMState MState{ mTrees = MNode _ state : _ } = nullMState state
nullMState _                                    = False

--
-- Monads
--

type MatchM = MaybeT EvalM

matchFail :: MatchM a
matchFail = MaybeT $ return Nothing
