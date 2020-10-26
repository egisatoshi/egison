{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wno-orphans        #-}

{- |
Module      : Language.Egison.PrettyPrint
Licence     : MIT

This module contains pretty printing for Egison syntax
-}

module Language.Egison.Pretty
    ( prettyTopExprs
    , prettyStr
    , showTSV
    ) where

import           Data.Foldable             (toList)
import           Data.List                 (intercalate)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String (renderString)
import           Text.Show.Unicode         (ushow)

import           Language.Egison.AST
import           Language.Egison.Data
import           Language.Egison.IExpr

--
-- Pretty printing for Non-S syntax
--

prettyTopExprs :: [TopExpr] -> Doc [TopExpr]
prettyTopExprs exprs = vsep $ punctuate line (map pretty exprs)

instance Pretty TopExpr where
  pretty (Define x (LambdaExpr args body)) =
    hsep (pretty "def" : pretty x : map pretty' args) <+> indentBlock (pretty ":=") [pretty body]
  pretty (Define x expr) =
    pretty "def" <+> pretty x <+> indentBlock (pretty ":=") [pretty expr]
  pretty (Test expr) = pretty expr
  pretty (LoadFile file) = pretty "loadFile" <+> pretty (show file)
  pretty (Load lib) = pretty "load" <+> pretty (show lib)
  pretty _ = error "Unsupported topexpr"

instance Pretty ConstantExpr where
  pretty (CharExpr x)    = viaShow x
  pretty (StringExpr x)  = pretty (ushow x)
  pretty (BoolExpr x)    = pretty x
  pretty (IntegerExpr x) = pretty x
  pretty (FloatExpr x)   = pretty x
  pretty SomethingExpr = pretty "something"
  pretty UndefinedExpr = pretty "undefined"

instance Pretty Expr where
  pretty (ConstantExpr c) = pretty c
  -- Use |viaShow| to correctly handle escaped characters
  pretty (VarExpr x)     = pretty x
  pretty FreshVarExpr    = pretty "#"
  pretty (IndexedExpr True e indices) = pretty' e <> cat (map pretty indices)
  pretty (IndexedExpr False e indices) = pretty' e <> pretty "..." <> cat (map pretty indices)
  pretty (SubrefsExpr b e1 e2) =
    applyLike [pretty "subrefs" <> (if b then pretty "!" else emptyDoc),
               pretty' e1, pretty' e2]
  pretty (SuprefsExpr b e1 e2) =
    applyLike [pretty "suprefs" <> (if b then pretty "!" else emptyDoc),
               pretty' e1, pretty' e2]
  pretty (UserrefsExpr b e1 e2) =
    applyLike [pretty "userRefs" <> (if b then pretty "!" else emptyDoc),
               pretty' e1, pretty' e2]

  pretty (TupleExpr xs) = tupled (map pretty xs)
  pretty (CollectionExpr xs)
    | length xs < 20 = list (map pretty xs)
    | otherwise      =
      pretty "[" <> align (fillSepAtom (punctuate comma (map pretty xs))) <> pretty "]"
  pretty (HashExpr xs)   = listoid "{|" "|}" (map (\(x, y) -> tupled [pretty x, pretty y]) xs)
  pretty (VectorExpr xs) = listoid "[|" "|]" (map pretty xs)

  pretty (LambdaExpr xs e) =
    lambdaLike (pretty "\\") (map pretty xs) (pretty "->") (pretty e)
  pretty (MemoizedLambdaExpr xs e)  =
    lambdaLike (pretty "memoizedLambda ") (map pretty xs) (pretty "->") (pretty e)
  pretty (CambdaExpr x e) =
    indentBlock (pretty "cambda" <+> pretty x <+> pretty "->") [pretty e]
  pretty (PatternFunctionExpr xs p) =
    lambdaLike (pretty "\\") (map pretty xs) (pretty "=>") (pretty p)

  pretty (IfExpr x y z) =
    indentBlock (pretty "if" <+> pretty x)
      [pretty "then" <+> pretty y, pretty "else" <+> pretty z]
  pretty (LetRecExpr bindings body) =
    hang 1 (pretty "let" <+> align (vsep (map pretty bindings)) <> hardline <> pretty "in" <+> align (pretty body))
  pretty (WithSymbolsExpr xs e) =
    indentBlock (pretty "withSymbols" <+> list (map pretty xs)) [pretty e]

  pretty (MatchExpr BFSMode tgt matcher clauses) =
    nest 2 (pretty "match"       <+> pretty tgt <+> prettyMatch matcher clauses)
  pretty (MatchExpr DFSMode tgt matcher clauses) =
    nest 2 (pretty "matchDFS"    <+> pretty tgt <+> prettyMatch matcher clauses)
  pretty (MatchAllExpr BFSMode tgt matcher clauses) =
    nest 2 (pretty "matchAll"    <+> pretty tgt <+> prettyMatch matcher clauses)
  pretty (MatchAllExpr DFSMode tgt matcher clauses) =
    nest 2 (pretty "matchAllDFS" <+> pretty tgt <+> prettyMatch matcher clauses)
  pretty (MatchLambdaExpr matcher clauses) =
    nest 2 (pretty "\\match"     <+> prettyMatch matcher clauses)
  pretty (MatchAllLambdaExpr matcher clauses) =
    nest 2 (pretty "\\matchAll"  <+> prettyMatch matcher clauses)

  pretty (MatcherExpr patDefs) =
    nest 2 (pretty "matcher" <> hardline <> align (vsep (map prettyPatDef patDefs)))
      where
        prettyPatDef (pppat, expr, body) =
          nest 2 (pipe <+> pretty pppat <+> pretty "as" <+>
            group (pretty expr) <+> pretty "with" <> hardline <>
              align (vsep (map prettyPatBody body)))
        prettyPatBody (pdpat, expr) =
          indentBlock (pipe <+> align (pretty pdpat) <+> pretty "->") [pretty expr]

  pretty (AlgebraicDataMatcherExpr patDefs) =
    nest 2 (pretty "algebraicDataMatcher" <> hardline <> align (vsep (map prettyPatDef patDefs)))
      where
        prettyPatDef (name, exprs) = pipe <+> hsep (pretty name : map pretty exprs)

  pretty (QuoteExpr e) = squote <> pretty' e
  pretty (QuoteSymbolExpr e) = pretty '`' <> pretty' e

  pretty (PrefixExpr op x@(ConstantExpr (IntegerExpr _))) = pretty op <> pretty x
  pretty (PrefixExpr op x)
    | isAtomOrApp x = pretty op <+> pretty x
    | otherwise     = pretty op <+> parens (pretty x)
  -- (x1 op' x2) op y
  pretty (InfixExpr op x@(InfixExpr op' _ _) y) =
    if priority op > priority op' || priority op == priority op' && assoc op == InfixR
       then parens (pretty x) <+> pretty op <> infixRight (pretty'' y)
       else pretty x          <+> pretty op <> infixRight (pretty'' y)
  -- x op (y1 op' y2)
  pretty (InfixExpr op x y@(InfixExpr op' _ _)) =
    if priority op > priority op' || priority op == priority op' && assoc op == InfixL
       then pretty'' x <+> pretty op <> infixRight (parens (pretty y))
       else pretty'' x <+> pretty op <> infixRight (pretty y)
  pretty (InfixExpr op x y) =
    pretty'' x <+> pretty op <> infixRight (pretty'' y)
  pretty (SectionExpr op Nothing Nothing) = parens (pretty op)
  pretty (SectionExpr op (Just x) Nothing) = parens (pretty x <+> pretty op)
  pretty (SectionExpr op Nothing (Just x)) = parens (pretty op <+> pretty x)

  pretty (DoExpr [] y) = pretty "do" <+> pretty y
  pretty (DoExpr xs (ApplyExpr (VarExpr "return") [])) =
    pretty "do" <+> align (hsepHard (map prettyDoBinds xs))
  pretty (DoExpr xs y) = pretty "do" <+> align (hsepHard (map prettyDoBinds xs ++ [pretty y]))

  pretty (SeqExpr e1 e2) = applyLike [pretty "seq", pretty' e1, pretty' e2]
  pretty (ApplyExpr x ys) = applyLike (map pretty' (x : ys))
  pretty (CApplyExpr e1 e2) = applyLike [pretty "capply", pretty' e1, pretty' e2]
  pretty (AnonParamFuncExpr n e) = pretty n <> pretty '#' <> pretty' e
  pretty (AnonParamExpr n) = pretty '%' <> pretty n

  pretty (GenerateTensorExpr gen shape) =
    applyLike [pretty "generateTensor", pretty' gen, pretty' shape]
  pretty (TensorExpr e1 e2) =
    applyLike [pretty "tensor", pretty' e1, pretty' e2]
  pretty (TensorContractExpr e1) =
    applyLike [pretty "contract", pretty' e1]
  pretty (TensorMapExpr e1 e2) =
    applyLike [pretty "tensorMap", pretty' e1, pretty' e2]
  pretty (TensorMap2Expr e1 e2 e3) =
    applyLike [pretty "tensorMap2", pretty' e1, pretty' e2, pretty' e3]
  pretty (TransposeExpr e1 e2) =
    applyLike [pretty "transpose", pretty' e1, pretty' e2]
  pretty (FlipIndicesExpr _) = error "unreachable"

  pretty (FunctionExpr xs) = pretty "function" <+> tupled (map pretty xs)

  pretty p = pretty (show p)

instance (Pretty a, Complex a) => Pretty (Arg a) where
  pretty (ScalarArg x)         = pretty "$" <> pretty' x
  pretty (InvertedScalarArg x) = pretty "*$" <> pretty' x
  pretty (TensorArg x)         = pretty x

instance Pretty ArgPattern where
  pretty APWildCard              = pretty "_"
  pretty (APPatVar x)            = pretty x
  pretty (APInductivePat x args) = applyLike (pretty x : map pretty' args)
  pretty (APTuplePat args)       = tupled (map pretty args)
  pretty APEmptyPat              = pretty "[]"
  pretty (APConsPat arg1 arg2)   = pretty'' arg1 <+> pretty "::" <+> pretty'' arg2
  pretty (APSnocPat arg1 arg2)   = applyLike [pretty "snoc", pretty' arg1, pretty' arg2]

instance Pretty VarWithIndices where
  pretty (VarWithIndices xs is) = pretty xs <> hcat (map pretty is)

instance Pretty VarIndex where
  pretty (VSubscript x)   = pretty ('_' : x)
  pretty (VSuperscript x) = pretty ('~' : x)
  pretty (VSymmScripts xs)     = pretty '{' <> hcat (map pretty xs) <> pretty '}'
  pretty (VAntiSymmScripts xs) = pretty '[' <> hcat (map pretty xs) <> pretty ']'

instance Pretty BindingExpr where
  pretty (Bind (PDPatVar f) (LambdaExpr args body)) =
    hsep (pretty f : map pretty' args) <+> indentBlock (pretty ":=") [pretty body]
  pretty (Bind pat expr) = pretty pat <+> pretty ":=" <+> align (pretty expr)
  pretty (BindWithIndices var expr) = pretty var <+> pretty ":=" <+> align (pretty expr)

instance {-# OVERLAPPING #-} Pretty MatchClause where
  pretty (pat, expr) =
    pipe <+> align (pretty pat) <+> indentBlock (pretty "->") [pretty expr]

instance {-# OVERLAPPING #-} Pretty (IndexExpr String) where -- for 'VarWithIndices'
  pretty (Superscript s)  = pretty ("~" ++ s)
  pretty (Subscript s)    = pretty ("_" ++ s)
  pretty (SupSubscript s) = pretty ("~_" ++ s)
  pretty (Userscript i)   = pretty ("|" ++ show i)
  pretty _                = undefined

instance (Pretty a, Complex a) => Pretty (IndexExpr a) where
  pretty (Subscript i) = pretty '_' <> pretty' i
  pretty (Superscript i) = pretty '~' <> pretty' i
  pretty (SupSubscript i) = pretty "~_" <> pretty' i
  pretty (MultiSubscript i j) = pretty '_' <> pretty' i <> pretty "..._" <> pretty' j
  pretty (MultiSuperscript i j) = pretty '~' <> pretty' i <> pretty "...~" <> pretty' j
  pretty (Userscript i) = pretty '|' <> pretty' i

instance Pretty Pattern where
  pretty WildCard     = pretty "_"
  pretty (PatVar x)   = pretty "$" <> pretty x
  pretty (ValuePat v) = pretty "#" <> pretty' v
  pretty (PredPat v)  = pretty "?" <> pretty' v
  pretty (IndexedPat p indices) =
    pretty p <> hcat (map (\i -> pretty '_' <> pretty' i) indices)
  pretty (LetPat binds pat) =
    pretty "let" <+> align (vsep (map pretty binds)) <+> pretty "in" <+> pretty pat
  -- (p11 op' p12) op p2
  pretty (InfixPat op p1@(InfixPat op' _ _) p2) =
    if priority op > priority op' || priority op == priority op' && assoc op == InfixR
       then parens (pretty p1) <+> pretty (repr op) <+> pretty'' p2
       else pretty p1          <+> pretty (repr op) <+> pretty'' p2
  -- p1 op (p21 op' p22)
  pretty (InfixPat op p1 p2@(InfixPat op' _ _)) =
    if priority op > priority op' || priority op == priority op' && assoc op == InfixL
       then pretty'' p1 <+> pretty (repr op) <+> parens (pretty p2)
       else pretty'' p1 <+> pretty (repr op) <+> pretty p2
  pretty (InfixPat op p1 p2) = pretty'' p1 <+> pretty (repr op) <+> pretty'' p2
  pretty (NotPat pat) = pretty "!" <> pretty' pat
  pretty (TuplePat pats) = tupled $ map pretty pats
  pretty (InductivePat "nil" []) = pretty "[]"
  pretty (InductivePat "::" [p, InductivePat "nil" []]) = pretty "[" <> pretty p <> pretty "]"
  pretty (InductivePat ctor xs) = hsep (pretty ctor : map pretty' xs)
  pretty (LoopPat i range p1 p2) =
    hang 2 (pretty "loop" <+> pretty '$' <> pretty i <+> pretty range <>
      flatAlt (hardline <> group (pretty' p1) <> hardline <> group (pretty' p2))
              (space <> pretty' p1 <+> pretty' p2))
  pretty ContPat = pretty "..."
  pretty (PApplyPat fn ps) = applyLike (pretty' fn : map pretty' ps)
  pretty (VarPat x) = pretty ('~' : x)
  pretty SeqNilPat = pretty "{}"
  pretty (SeqConsPat p1 p2) = listoid "{" "}" (f p1 p2)
    where
      f p1 SeqNilPat = [pretty p1]
      f p1 (SeqConsPat p2 p3) = pretty p1 : f p2 p3
      f p1 p2 = [pretty p1, pretty p2]
  pretty LaterPatVar = pretty "@"
  pretty (DApplyPat p ps) = applyLike (map pretty' (p : ps))
  pretty e            = pretty (show e)

instance {-# OVERLAPPING #-} Pretty LoopRange where
  pretty (LoopRange from (ApplyExpr (VarExpr "from")
                                    [InfixExpr Op{ repr = "-'" } _ (ConstantExpr (IntegerExpr 1))]) pat) =
    tupled [pretty from, pretty pat]
  pretty (LoopRange from to pat) = tupled [pretty from, pretty to, pretty pat]

instance Pretty PrimitivePatPattern where
  pretty PPWildCard     = pretty "_"
  pretty PPPatVar       = pretty "$"
  pretty (PPValuePat x) = pretty ('#' : '$' : x)
  pretty (PPInductivePat x pppats) = hsep (pretty x : map pretty pppats)
  pretty (PPTuplePat pppats) = tupled (map pretty pppats)

instance Pretty PrimitiveDataPattern where
  pretty PDWildCard   = pretty "_"
  pretty (PDPatVar x) = pretty x
  pretty (PDInductivePat x pdpats) = applyLike (pretty x : map pretty' pdpats)
  pretty (PDTuplePat pdpats) = tupled (map pretty pdpats)
  pretty PDEmptyPat = pretty "[]"
  pretty (PDConsPat pdp1 pdp2) = pretty'' pdp1 <+> pretty "::" <+> pretty'' pdp2
  pretty (PDSnocPat pdp1 pdp2) = applyLike [pretty "snoc", pretty' pdp1, pretty' pdp2]
  pretty (PDConstantPat expr) = pretty expr

instance Pretty Op where
  pretty op | isWedge op = pretty ("!" ++ repr op)
            | otherwise  = pretty (repr op)

instance Pretty IExpr where
  pretty = undefined

instance Complex IExpr where
  isAtom = undefined
  isAtomOrApp = undefined
  isInfix = undefined

class Complex a where
  isAtom :: a -> Bool
  isAtomOrApp :: a -> Bool
  isInfix :: a -> Bool

instance Complex Expr where
  isAtom (ConstantExpr (IntegerExpr i)) | i < 0  = False
  isAtom PrefixExpr{}             = False
  isAtom InfixExpr{}              = False
  isAtom (ApplyExpr _ [])         = True
  isAtom ApplyExpr{}              = False
  isAtom CApplyExpr{}             = False
  isAtom LambdaExpr{}             = False
  isAtom MemoizedLambdaExpr{}     = False
  isAtom CambdaExpr{}             = False
  isAtom PatternFunctionExpr{}    = False
  isAtom IfExpr{}                 = False
  isAtom LetRecExpr{}             = False
  isAtom SubrefsExpr{}            = False
  isAtom SuprefsExpr{}            = False
  isAtom UserrefsExpr{}           = False
  isAtom WithSymbolsExpr{}        = False
  isAtom MatchExpr{}              = False
  isAtom MatchAllExpr{}           = False
  isAtom MatchLambdaExpr{}        = False
  isAtom MatchAllLambdaExpr{}     = False
  isAtom MatcherExpr{}            = False
  isAtom AlgebraicDataMatcherExpr{} = False
  isAtom GenerateTensorExpr{}     = False
  isAtom TensorExpr{}             = False
  isAtom FunctionExpr{}           = False
  isAtom TensorContractExpr{}     = False
  isAtom TensorMapExpr{}          = False
  isAtom TensorMap2Expr{}         = False
  isAtom TransposeExpr{}          = False
  isAtom _                        = True

  isAtomOrApp ApplyExpr{}         = True
  isAtomOrApp e                   = isAtom e

  isInfix InfixExpr{}             = True
  isInfix _                       = False

instance Complex a => Complex (Arg a) where
  isAtom (TensorArg x) = isAtom x
  isAtom _             = True

  isAtomOrApp = isAtom

  isInfix _ = False

instance Complex ArgPattern where
  isAtom (APInductivePat _ []) = True
  isAtom APInductivePat{}      = False
  isAtom APConsPat{}           = False
  isAtom APSnocPat{}           = False
  isAtom _                     = True

  isAtomOrApp = isAtom
  isInfix _ = False

instance Complex Pattern where
  isAtom LetPat{}            = False
  isAtom (InductivePat _ []) = True
  isAtom (InductivePat _ _)  = False
  isAtom InfixPat{}          = False
  isAtom LoopPat{}           = False
  isAtom (PApplyPat _ [])    = True
  isAtom (PApplyPat _ _)     = False
  isAtom _                   = True

  isAtomOrApp PApplyPat{}    = True
  isAtomOrApp InductivePat{} = True
  isAtomOrApp e              = isAtom e

  isInfix InfixPat{} = True
  isInfix _          = False

instance Complex PrimitiveDataPattern where
  isAtom (PDInductivePat _ []) = True
  isAtom (PDInductivePat _ _)  = False
  isAtom PDConsPat{}           = False
  isAtom PDSnocPat{}           = False
  isAtom _                     = True

  isAtomOrApp PDInductivePat{} = True
  isAtomOrApp PDSnocPat{}      = True
  isAtomOrApp e                = isAtom e

  isInfix PDConsPat{} = True
  isInfix _           = False

pretty' :: (Pretty a, Complex a) => a -> Doc ann
pretty' x | isAtom x  = pretty x
          | otherwise = parens $ pretty x

pretty'' :: (Pretty a, Complex a) => a -> Doc ann
pretty'' x | isAtomOrApp x || isInfix x = pretty x
           | otherwise                  = parens $ pretty x

-- Display "hoge" instead of "() := hoge"
prettyDoBinds :: BindingExpr -> Doc ann
prettyDoBinds (Bind (PDTuplePat []) expr) = pretty expr
prettyDoBinds bind = pretty "let" <+> pretty bind

prettyMatch :: Expr -> [MatchClause] -> Doc ann
prettyMatch matcher clauses =
  pretty "as" <> group (flatAlt (hardline <> pretty matcher) (space <> pretty matcher) <+> pretty "with") <> hardline <>
    align (vsep (map pretty clauses))

listoid :: String -> String -> [Doc ann] -> Doc ann
listoid lp rp elems =
  encloseSep (pretty lp) (pretty rp) (comma <> space) elems

-- Just like |fillSep|, but does not break the atomicity of grouped Docs
fillSepAtom :: [Doc ann] -> Doc ann
fillSepAtom [] = emptyDoc
fillSepAtom [x] = x
fillSepAtom (x:xs) = x <> fillSepAtom' xs
  where
    fillSepAtom' [] = emptyDoc
    fillSepAtom' (x:xs) =
      group (flatAlt (hardline <> x) (space <> x)) <> fillSepAtom' xs

indentBlock :: Doc ann -> [Doc ann] -> Doc ann
indentBlock header bodies =
  group (nest 2 (header <> flatAlt (hardline <> hsepHard bodies) (space <> hsep bodies)))

hsepHard :: [Doc ann] -> Doc ann
hsepHard = concatWith (\x y -> x <> hardline <> y)

lambdaLike :: Doc ann -> [Doc ann] -> Doc ann -> Doc ann -> Doc ann
lambdaLike start [] arrow body =
  indentBlock (start <> pretty "()" <+> arrow) [body]
lambdaLike start args arrow body =
  indentBlock (start <> hsep args <+> arrow) [body]

applyLike :: [Doc ann] -> Doc ann
applyLike = hang 2 . sep . map group

-- Tests if the argument can be printed in a single line, and if not,
-- inserts a line break before printing it.
-- This is useful for nicely printing infix expressions.
infixRight :: Doc ann -> Doc ann
infixRight p = group (flatAlt (hardline <> p) (space <> p))

showTSV :: EgisonValue -> String
showTSV (Tuple (val:vals)) = foldl (\r x -> r ++ "\t" ++ x) (show val) (map show vals)
showTSV (Collection vals) = intercalate "\t" (map show (toList vals))
showTSV val = show val

--
-- Pretty printer for error messages
--

prettyStr :: Pretty a => a -> String
prettyStr = renderString . layoutPretty (LayoutOptions Unbounded) . pretty
