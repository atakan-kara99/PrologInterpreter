module Subst
    ( Subst(Subst), empty, single, apply, compose, restrictTo, pretty, allVars
    ) where

import Type
import Pretty
import Vars (Vars, allVars)
import Data.List (intercalate)

-- declared data type Subst
data Subst = Subst [(VarName, Term)]
  deriving Show

-- construct empty substitution
empty :: Subst
empty = Subst []

-- construct a substitution with single substitution
single :: VarName -> Term -> Subst
single v t = Subst [(v,t)]

-- apply substitution on a term
apply :: Subst -> Term -> Term
apply (Subst []) x                = x
apply (Subst ((v,t):ss)) (Var v1) = if v == v1
                                      then t
                                      else apply (Subst ss) (Var v1)
apply s (Comb x ts)               = Comb x (map (apply s) ts)

-- compose two Subst together
compose :: Subst -> Subst -> Subst
compose (Subst []) s1           = s1
compose s2 (Subst [])           = s2
compose (Subst ss2) (Subst ss1) =
  Subst ((map (\(x,y) -> (x, apply (Subst rest) y)) ss1) ++ rest)
    where
      rest = restrict ss2 (vars ss1)

-- if the left pair of a Subst is already in the given subst it will delete
restrict :: [(VarName, Term)] -> [VarName] -> [(VarName, Term)]
restrict [] _          = []
restrict ((v,t):vts) ys = if elem v ys
                            then restrict vts ys
                            else [(v,t)] ++ restrict vts ys

-- all vars of the left pair of a Subst
vars :: [(VarName, Term)] -> [VarName]
vars []         = []
vars ((v,_):ss) = [v] ++ vars ss

-- restricts substitution to a given list of Vars
restrictTo :: [VarName] -> Subst -> Subst
restrictTo [] _                  = empty
restrictTo _ (Subst [])          = empty
restrictTo xs (Subst ((v,t):ss)) =
  if elem v xs
    then Subst ([(v,t)] ++ (\(Subst x) -> x) (restrictTo xs (Subst ss)))
    else restrictTo xs (Subst ss)

-- represent substitution correctly
instance Pretty Subst where
  pretty (Subst [])      = "{}"
  pretty (Subst [(v,t)]) = "{" ++ prettyPair (v,t) ++  "}"
  pretty (Subst ss)      = "{" ++ intercalate ", " (map prettyPair ss)  ++ "}"

-- convert a pair into a string
prettyPair :: (String, Term) -> String
prettyPair (x,y) = x ++ " -> " ++ pretty y

-- return list of Varnames in a Subst
instance Vars Subst where
  allVars (Subst [])         = []
  allVars (Subst ((v,t):ss)) = [v] ++ allVars t ++ allVars (Subst ss)
