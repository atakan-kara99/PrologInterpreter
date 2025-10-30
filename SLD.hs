module SLD
    ( sld, bfs, dfs, solve
    ) where

import Type
import Subst (Subst, apply, empty, compose, restrictTo)
import Uni (unify)
import Data.Maybe (isNothing, fromJust)
import Pretty (Pretty, pretty)
import Vars (allVars)
import Rename

-- type of Strategy
type Strategy = SLDTree -> [Subst]

-- data for sldTrees
data SLDTree = Node Goal [(Subst, SLDTree)]
  deriving Show

-- start of the construction of a sldTree with sld resolution
sld :: Prog -> Goal -> SLDTree
sld (Prog r) (Goal g) = Node (Goal g) (allChildren r g)

-- zips all substitutions and trees together
-- one pair is a "childNode"
allChildren :: [Rule] -> [Term] -> [(Subst, SLDTree)]
allChildren rs gs = zip (rulesToSubst hgs (findRules hgs renamedProg))
                        (sldTrees (Prog rs) (findRules hgs renamedProg) (rulesToSubst hgs (findRules hgs renamedProg)) gs)
  where
    renamedProg = map (\r -> rename r (allVars gs)) rs
    hgs         = head gs

-- finds all rules in the program that are unificatable with the goal
findRules :: Term -> [Rule] -> [Rule]
findRules _ []                 = []
findRules g ((Rule t1 t1s):rs) = if isNothing (unify g t1)
                                   then findRules g rs
                                   else [(Rule t1 t1s)] ++ findRules g rs

-- calculatescthe substitutions to the rules a list
rulesToSubst :: Term -> [Rule] -> [Subst]
rulesToSubst _ []               = []
rulesToSubst g ((Rule t1 _):rs) = [fromJust (unify g t1)] ++ rulesToSubst g rs

-- recursive calculations of all sldTrees as a list
sldTrees :: Prog -> [Rule] -> [Subst] -> [Term] -> [SLDTree]
sldTrees _ []               []     _      = []
sldTrees p ((Rule _ ts):rs) (s:ss) [g]    = if null ts
                                              then [Node (Goal []) []] ++ sldTrees p rs ss [g]
                                              else [sld p (Goal (map (apply s) ts))] ++ sldTrees p rs ss [g]
sldTrees p ((Rule _ ts):rs) (s:ss) (_:gs) = if null ts
                                              then [sld p (Goal (map (apply s) gs))] ++ sldTrees p rs ss gs
                                              else [sld p (Goal (map (apply s) ts))] ++ sldTrees p rs ss gs
sldTrees _ _                _      _      = []

-- a bad pretty instance for sldTrees
-- instance Pretty SLDTree where
--   pretty (Node (Goal []) [])            = "success \n"
--   pretty (Node (Goal g) [])             = (concatMap pretty g) ++ " fail \n"
--   pretty (Node (Goal g) ((s,sLD):pair)) = (concatMap pretty g) ++ "." ++ "\n" ++ prettyP (s,sLD) ++ concatMap prettyP pair
--    where
--      prettyP (s1,slD) = pretty s1 ++ "\n" ++ pretty slD

-- start of dfs algorithm
dfs :: Strategy
dfs (Node _ (ss)) = substsToSubst (map (\(s,_) -> s) ss) (map algoDFS (map (\(_,sLD) -> sLD) ss))

-- recursive function for dfs
algoDFS :: SLDTree -> Subst
algoDFS (Node _ ((s,(Node (Goal []) [])):_))  = s
algoDFS (Node (Goal g) ((_,(Node _ [])):ss2)) = if null ss2
                                                  then empty
                                                  else algoDFS (Node (Goal g) ss2)
algoDFS (Node _ ((s,sLD):_))                  = compose s (algoDFS sLD)
algoDFS _                                     = empty

-- combine to lists of substitutions to one
-- by composing the first ones together then the second and so on
substsToSubst :: [Subst] -> [Subst] -> [Subst]
substsToSubst (s:ss) (s1:ss1) = [compose s s1] ++ substsToSubst ss ss1
substsToSubst _ _             = []

-- start of bfs algorithm
bfs :: Strategy
bfs (Node (Goal g) (ss)) = algoBFS (queue [] (Node (Goal g) (ss)))

-- recursive function for bfs with a queue
algoBFS :: [(Subst, SLDTree)] -> [Subst]
algoBFS ((s,(Node (Goal []) [])):qs) = [s] ++ algoBFS qs
algoBFS ((_,(Node _ [])):qs)         = algoBFS qs
algoBFS ((s,sLD):qs)                 = algoBFS (compOnQueue s (queue qs sLD))
algoBFS []                           = []

-- create a queue or add a queue to another one
queue :: [(Subst, SLDTree)] -> SLDTree -> [(Subst, SLDTree)]
queue [] (Node _ (ss)) = ss
queue qs (Node _ (ss)) = qs ++ ss

-- compose the substitution of the vater to all child substitutions
compOnQueue :: Subst -> [(Subst, SLDTree)] -> [(Subst, SLDTree)]
compOnQueue _ []            = []
compOnQueue s1 ((s,sLD):ss) = [(compose s1 s, sLD)] ++ compOnQueue s1 ss

-- solves a goal to a program with a given Strategy
solve :: Strategy -> Prog -> Goal -> [Subst]
solve s p g = map (restrictTo (allVars g)) (s (sld p g))

-- putStrLn (pretty (sld (Prog [Rule (Comb "p" [Var "X", Var "Z"]) [(Comb "q" [Var "X", Var "Y"]), (Comb "p" [Var "Y", Var "Z"])], Rule (Comb "p" [Var "X", Var "X"]) [], Rule (Comb "q" [Comb "a" [], Comb "b" []]) []]) (Goal [Comb "p" [Var "S", Comb "b" []]])))
-- (Node (Goal [Comb "p" [Var "S",Comb "b" []]]) [(Subst [("X",Var "S"),("Z",Comb "b" [])],Node (Goal [Comb "q" [Var "S",Var "Y"],Comb "p" [Var "Y",Comb "b" []]]) [(Subst [("S",Comb "a" []),("Y",Comb "b" [])],Node (Goal []) [])]),(Subst [("X",Comb "b" []),("S",Comb "b" [])],Node (Goal []) [])])
