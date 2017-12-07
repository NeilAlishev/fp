module PreviousSem where

import Types
import Data.Unique
import System.IO.Unsafe (unsafePerformIO)

-- some useful functions from the previous semester work

alpha :: Term -> Term
alpha sym_arg@(Sym _) = sym_arg
alpha (App term1 term2) = App (alpha term1) (alpha term2)
alpha (Lam sym parameter_type term) =
  let new_sym_name = get_unique_sym sym
  in Lam new_sym_name parameter_type (_change (alpha term) sym new_sym_name)

_change (Sym sym) sym_to_change new_sym_name
  | sym == sym_to_change = (Sym new_sym_name)
  | otherwise = Sym sym
_change (App term1 term2) sym_to_change new_sym_name =
  App (_change term1 sym_to_change new_sym_name) (_change term2 sym_to_change new_sym_name)

_change (Lam sym parameter_type term) sym_to_change new_sym_name =
  Lam sym parameter_type (_change term sym_to_change new_sym_name)

get_unique_sym :: Symbol -> Symbol
get_unique_sym x =
    let unique = unsafePerformIO newUnique
    in x ++ show (hashUnique unique)