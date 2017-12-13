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

alpha (Iff term1 term2 term3) = Iff (alpha term1) (alpha term2) (alpha term3)

alpha (Add term1 term2) = Add (alpha term1) (alpha term2)
alpha (Mult term1 term2) = Mult (alpha term1) (alpha term2)
alpha (And term1 term2) = And (alpha term1) (alpha term2)
alpha (Or term1 term2) = Or (alpha term1) (alpha term2)
alpha (Pair term1 term2) = Pair (alpha term1) (alpha term2)
alpha (Cons term1 term2) = Cons (alpha term1) (alpha term2)

alpha (Not term) = Not (alpha term)
alpha (Fst term) = Fst (alpha term)
alpha (Snd term) = Snd (alpha term)
alpha (IsNil term) = IsNil (alpha term)
alpha (Head term) = Head (alpha term)
alpha (Tail term) = Tail (alpha term)

alpha (Natural x) = Natural x
alpha (Boolean b) = Boolean b
alpha (Nil t) = Nil t

_change (Sym sym) sym_to_change new_sym_name
  | sym == sym_to_change = (Sym new_sym_name)
  | otherwise = Sym sym

_change (App term1 term2) sym_to_change new_sym_name =
  App (_change term1 sym_to_change new_sym_name) (_change term2 sym_to_change new_sym_name)

_change (Lam sym parameter_type term) sym_to_change new_sym_name =
  Lam sym parameter_type (_change term sym_to_change new_sym_name)

_change (Iff term1 term2 term3) sym_to_change new_sym_name =
    Iff (_change term1 sym_to_change new_sym_name) (_change term2 sym_to_change new_sym_name)
        (_change term3 sym_to_change new_sym_name)

_change (Add term1 term2) sym_to_change new_sym_name =
    Add (_change term1 sym_to_change new_sym_name) (_change term2 sym_to_change new_sym_name)

_change (Mult term1 term2) sym_to_change new_sym_name =
    Mult (_change term1 sym_to_change new_sym_name) (_change term2 sym_to_change new_sym_name)

_change (And term1 term2) sym_to_change new_sym_name =
    And (_change term1 sym_to_change new_sym_name) (_change term2 sym_to_change new_sym_name)

_change (Or term1 term2) sym_to_change new_sym_name =
    Or (_change term1 sym_to_change new_sym_name) (_change term2 sym_to_change new_sym_name)

_change (Pair term1 term2) sym_to_change new_sym_name =
    Pair (_change term1 sym_to_change new_sym_name) (_change term2 sym_to_change new_sym_name)

_change (Cons term1 term2) sym_to_change new_sym_name =
    Cons (_change term1 sym_to_change new_sym_name) (_change term2 sym_to_change new_sym_name)

_change (Not term) sym_to_change new_sym_name =
    Not (_change term sym_to_change new_sym_name)

_change (Fst term) sym_to_change new_sym_name =
    Fst (_change term sym_to_change new_sym_name)

_change (Snd term) sym_to_change new_sym_name =
    Snd (_change term sym_to_change new_sym_name)

_change (IsNil term) sym_to_change new_sym_name =
    IsNil (_change term sym_to_change new_sym_name)

_change (Head term) sym_to_change new_sym_name =
    Head (_change term sym_to_change new_sym_name)

_change (Tail term) sym_to_change new_sym_name =
    Tail (_change term sym_to_change new_sym_name)

_change (Natural x) sym_to_change new_sym_name = Natural x
_change (Boolean b) sym_to_change new_sym_name = Boolean b
_chage (Nil t) sym_to_change new_sym_name = Nil t

get_unique_sym :: Symbol -> Symbol
get_unique_sym x =
    let unique = unsafePerformIO newUnique
    in x ++ show (hashUnique unique)