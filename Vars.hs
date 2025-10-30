module Vars
    ( Vars, allVars, freshVars
    ) where

import Type

-- declare typeclass Vars with allVars method
class Vars a where
  allVars :: a -> [VarName]

-- return Varnames in a Term
instance Vars Term where
  allVars (Var x)     = [x]
  allVars (Comb _ ys) = lVars ys

-- return Varnames in a Rule
instance Vars Rule where
  allVars (Rule t ts) =  allVars t ++ lVars ts

-- return Varnames in a Prog
instance Vars Prog where
  allVars (Prog r) = lVars r

-- return Varnames in a Goal
instance Vars Goal where
  allVars (Goal ts) = lVars ts

-- apply allVars at all further elements if allVars is defined for the data
lVars :: Vars a => [a] -> [VarName]
lVars l = concatMap allVars l

instance Vars a => Vars [a] where
  allVars = lVars
  
-- generate infinite list of variable names
-- first char has to be uppercase letter or underscore
-- next letters have to be a-z, A-Z, _, 0-9
freshVars :: [VarName]
freshVars = filter (/="_") [ c : s | s <- "" : from2, c <- ['A'..'Z'] ++ ['_'] ]
 where
  -- generate list with all valid characters
  from2 :: [String]
  from2 = [ c : s | s <- "" : from2, c <- ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'] ]
