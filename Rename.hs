module Rename
    ( rename
    ) where

import Type
import Vars (allVars, freshVars)
import Subst (Subst(Subst), apply)

-- start of the rename function
-- it returns a rule with alternative Varnames
rename :: Rule -> [VarName] -> Rule
rename r vs = algo r (vs ++ (allVars r)) []

-- recursive function for rename
-- vs: vorbidden vars ++ vars that are in my Rule ++ vars that are used
-- ss: vars that has been used
algo :: Rule -> [VarName] -> [VarName] -> Rule
algo r vs ss = if findVar r ss == "finished"
                 then r
                 else algo (applyOnRule (Subst [((findVar r ss), Var freshV)]) r)
                           (vs ++ [freshV])
                           (ss ++ [freshV])
  where
    freshV = fresh vs 0

-- finds the first var that was not renamed already and is not an anonymous var
findVar :: Rule -> [VarName] -> VarName
findVar r ss = extractVar (filter (/="_") (allVars r)) ss 0

-- finds the next var to rename that was not renamed already
extractVar :: [VarName] -> [VarName] -> Int -> VarName
extractVar v1 ss n = if n >= (length v1)
                       then "finished"
                       else if elem (v1 !! n) ss
                              then extractVar v1 ss (n+1)
                              else v1 !! n

-- returns a fresh var that is not in the vorbidden list
-- starts with teh letter 'A'
fresh :: [VarName] -> Int -> VarName
fresh vs n = if elem (freshVars !! n) vs
               then fresh vs (n+1)
               else freshVars !! n

-- apply a substitution to a rule
applyOnRule :: Subst -> Rule -> Rule
applyOnRule s (Rule t ts)                 = Rule (apply s t) (map (apply s) ts)
