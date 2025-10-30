module Pretty
    ( Pretty, pretty
    ) where

import Type
import Data.List (intercalate)

-- declare typeclass Pretty with pretty method
class Pretty a where
  pretty :: a -> String

-- Term instance to return prolog syntax correctly
instance Pretty Term where
  pretty (Var x)             = x
  pretty (Comb x [])         = x
  pretty (Comb "." [t1, t2]) = "[" ++ prettyList t1 t2 ++ "]"
   where
    -- convert two terms into listitems
    prettyList :: Term -> Term -> String
    prettyList t3 (Comb "[]" [])      = pretty t3
    prettyList t3 (Comb "." [t4, t5]) = pretty t3 ++ ", " ++  prettyList t4 t5
    prettyList t3 t4                  = pretty t3 ++ "|" ++ pretty t4
  -- if pattern is not matched earlier eg.: x(y1,y2:ys)
  pretty (Comb x ys)         = x ++ "(" ++ intercalate ", " (map pretty ys) ++ ")"
