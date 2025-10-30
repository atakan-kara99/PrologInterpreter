module Uni
    ( ds, unify
    ) where

import Type
import Subst (Subst(Subst), empty, single, apply, compose)
import Pretty
import Data.Maybe (isNothing)
import Vars (allVars)

-- calculates the disagreement-set of two terms, it ignores anonymous vars
ds :: Term -> Term -> Maybe (Term, Term)
ds (Var v1) (Var v2)         = if v1 /= v2 && v1 /= "_" && v2 /= "_"
                                 then Just ((Var v1), (Var v2))
                                 else Nothing
ds (Comb c1 t1) (Comb c2 t2) = if (pretty (Comb c1 t1)) /= (pretty (Comb c2 t2))
                                 then if c1 == c2 && length t1 == length t2
                                        then dsArgs t1 t2
                                        else Just ((Comb c1 t1), (Comb c2 t2))
                                 else Nothing
ds t1 t2                     = Just (t1,t2)

-- auxiliary funktion to calculate ds of terms that have the same amount of arguments
-- and have the same funkcion name
dsArgs :: [Term] -> [Term] -> Maybe (Term, Term)
dsArgs (x:xs) (y:ys) = if pretty x == pretty y || pretty x == "_" || pretty y == "_"
                         then dsArgs xs ys
                         else Just (x,y)
dsArgs _      _      = Nothing

-- start of the unification algorithm
unify :: Term -> Term -> Maybe Subst
unify t1 t2 = algo t1 t2 empty

-- recursive functionto cal the substitution if there is one
algo :: Term -> Term -> Subst -> Maybe (Subst)
algo t1 t2 s = if isNothing (ds (apply s t1) (apply s t2))
                 then Just s
                 else if pretty (fromDsToSubst (ds (apply s t1) (apply s t2))) == pretty empty ||
                         (\(Subst [(v,t)]) -> elem v (allVars t)) (fromDsToSubst (ds (apply s t1) (apply s t2)))
                        then Nothing
                        else algo (apply s t1) (apply s t2) (compose (fromDsToSubst (ds (apply s t1) (apply s t2))) s)

-- returns the substitution from a disagreement-set
fromDsToSubst :: Maybe (Term, Term) -> Subst
fromDsToSubst (Just (t, Var v)) = single v t
fromDsToSubst (Just (Var v,t))  = single v t
fromDsToSubst _                 = empty
