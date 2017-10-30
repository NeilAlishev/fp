-- TODO: declare signatures for all functions
-- TODO: refactor the code when all done
-- TODO: refactor everything using lambda@ syntax
-- Example:
-- _reduce (AppS term1 term2) lam@(LamS sym term) = AppS (_reduce term1 term2) lam
-- TODO: refactor everything using guards
-- bmiTell bmi
--     | bmi <= 18.5 = "You're underweight, you emo, you!"
--     | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
--     | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
--     | otherwise   = "You're a whale, congratulations!"

-- also can be inline:
-- max' a b | a > b = a | otherwise = b

module Main where
import Data.Maybe
import Data.Unique
import System.IO.Unsafe (unsafePerformIO)

newtype Symbol = Symbol { unSymbol :: String } deriving (Eq,Show,Read)

-- (1)
data TermS = SymS Symbol        -- x
           | LamS Symbol TermS  -- \x -> t
           | AppS TermS TermS   -- t1 t2
           deriving (Eq,Show,Read) -- ПОСМОТРЕТЬ КАК РАБОТАЕТ READ с коммандной строки

-- (1)
-- переименовать все переменные так, чтобы все они были разными.
-- first go max deep, then start changing variable names
alpha :: TermS -> TermS
alpha (SymS sym) = SymS sym
alpha (AppS term1 term2) = AppS (alpha term1) (alpha term2)
alpha (LamS sym term) =
  let new_sym_name = get_unique_sym sym
  in LamS new_sym_name (_change (alpha term) sym new_sym_name)

_change (SymS sym) sym_to_change new_sym_name
  | sym == sym_to_change = (SymS new_sym_name)
  | otherwise = SymS sym
_change (AppS term1 term2) sym_to_change new_sym_name =
  AppS (_change term1 sym_to_change new_sym_name) (_change term2 sym_to_change new_sym_name)

_change (LamS sym term) sym_to_change new_sym_name =
  LamS sym (_change term sym_to_change new_sym_name)

get_unique_sym :: Symbol -> Symbol
get_unique_sym (Symbol x) =
    let unique = unsafePerformIO newUnique
    in Symbol (x ++ show (hashUnique unique))

-- (1)
-- один шаг редукции, если это возможно. Стратегия вычислений - полная, т.е. редуцируются все возможные редексы.
beta :: TermS -> Maybe TermS

beta termS = do
    let reduced_term = _beta termS

    -- no reduction was done
    if termS == reduced_term
        then Nothing
    else
        Just reduced_term

_beta (SymS sym) = SymS sym
_beta (LamS sym term) = LamS sym (_beta term)
_beta (AppS term1 term2) = _reduce term1 term2

-- we can't reduce this right away
_reduce (AppS term1 term2) (LamS sym term) = AppS (_reduce term1 term2) (LamS sym term)
_reduce (LamS sym term) (AppS term1 term2) = AppS (LamS sym term) (_reduce term1 term2)
_reduce (AppS term1 term2) (SymS sym) = AppS (_reduce term1 term2) (SymS sym)
_reduce (SymS sym) (AppS term1 term2) = AppS (SymS sym) (_reduce term1 term2) -- this should go to _reduce Sym Sym
_reduce (AppS first_term1 first_term2) (AppS second_term1 second_term2) =
    -- always reduce first application
    AppS (_reduce first_term1 first_term2) (AppS second_term1 second_term2)

-- this is for Church numerals where s and z are unknown
_reduce (SymS s1) (SymS s2) = AppS (SymS s1) (SymS s2)

-- we can reduce this
_reduce (LamS sym term) (SymS arg) = _apply_sym term (SymS sym) (SymS arg)
_reduce (LamS sym term) (LamS sym_arg term_arg) = _apply_lam term (SymS sym) (LamS sym_arg term_arg)

_apply_sym :: TermS -> TermS -> TermS -> TermS
_apply_sym (SymS current_sym) (SymS sym) (SymS arg) =
    -- apply sym
    if current_sym == sym
        then SymS arg
    else
        SymS current_sym

-- traverse inside
_apply_sym (LamS current_sym term) (SymS sym) (SymS arg) = LamS current_sym (_apply_sym term (SymS sym) (SymS arg))
-- traverse inside
_apply_sym (AppS term1 term2) (SymS sym) (SymS arg) =
    AppS (_apply_sym term1 (SymS sym) (SymS arg)) (_apply_sym term2 (SymS sym) (SymS arg))

_apply_lam :: TermS -> TermS -> TermS -> TermS
_apply_lam (SymS current_sym) (SymS sym) (LamS sym_arg term_arg) =
    -- apply lam
    if current_sym == sym
        then LamS sym_arg term_arg
    else
        SymS current_sym

-- traverse inside
_apply_lam (LamS current_sym term) (SymS sym) (LamS sym_arg term_arg) =
    LamS current_sym (_apply_lam term (SymS sym) (LamS sym_arg term_arg))
-- traverse inside
_apply_lam (AppS term1 term2) (SymS sym) (LamS sym_arg term_arg) =
    AppS (_apply_lam term1 (SymS sym) (LamS sym_arg term_arg)) (_apply_lam term2 (SymS sym) (LamS sym_arg term_arg))

-- let
sym x = SymS (Symbol x)
lam x t = LamS (Symbol x) t
app t1 t2 = AppS t1 t2

test_term_alpha1 = lam "x" $ lam "y" $ (app (sym "x") (sym "y"))
-- test term from the repo
test_term_alpha2 = lam "x" $ lam "x" $ lam "x" $ app (app (app (lam "b" $ lam "f" $ lam "s" $ app (app (sym "b") (sym "f")) (sym "s")) (lam "x" $ lam "y" $ sym "x")) (lam "x" $ sym "x")) (lam "x" $ lam "y" $ sym "y")

test_term_beta1 = lam "x" $ app (app (lam "t" $ lam "f" $ sym "t") (sym "x")) (lam "z" $ sym "z")
test_term_beta2 = lam "x" $ app (lam "f" $ sym "x") (lam "z" $ sym "z")
test_term_beta3 = app (lam "x" $ sym "s") (sym "f")
test_term_beta4 = (lam "x" $ sym "x")
test_term_beta5 = app (app (lam "x" $ lam "f" $ sym "f") (sym "x1")) (app (lam "x" $ lam "f" $ sym "x") (sym "x1"))

-- test term from the repo
test_term_beta6 = lam "a" $ lam "b" $ lam "c" $ app (app (app (lam "d" $ lam "e" $ lam "f" $ app (app (sym "d") (sym "e")) (sym "f")) (lam "g" $ lam "h" $ sym "g")) (lam "i" $ sym "i")) (lam "j" $ lam "k" $ sym "k")

-- apply beta function until it returns 'Nothing'
full_beta :: TermS -> TermS
full_beta term = do
    let reduced_term = beta term

    if reduced_term == Nothing
        then term
    else
        full_beta (fromJust reduced_term)

data TermP = TermP TermS
           -- (4)
           | Natural Int
           | Plus TermP TermP
           | Mult TermP TermP
           -- (5*) 50%
           | Y TermP
           -- (5**) 50%
           -- mutually recursive
           -- (7)
           | Cons TermP TermP
           | Nil -- для пустого списка
           | IsNil TermP
           | Head TermP
           | Tail TermP
           deriving (Eq,Show,Read)

toTermS :: TermP -> TermS
toTermS = error "Not implemented!"

-- we can perform arithmetic operations only on Church numerals, not on arbitrary lambda expressions
test_term6 = Plus (Natural 1) (Natural 2)

zero = lam "s" $ lam "z" $ sym "z"
scc = lam "n" $ lam "s" $ lam "z" $ app (sym "s") (app (app (sym "n") (sym "s")) (sym "z"))

one = lam "s" $ lam "z" $ app (app (app scc zero) (sym "s")) (sym "z") -- works!
two = lam "s" $ lam "z" $ app (app (app scc one) (sym "s")) (sym "z") -- works!

-- TODO: implement toTermS (Natural x) using recursion and scc function
-- TODO: here return something like lam s $ lam z $ app (sym s) (sym z) = 1

-- convert natural number to the Church numeral
-- toTermS (Natural x) =

-- toTermS (Plus x1 x2) = _sum_church_numbers (toTermS x1) (toTermS x2)
-- toTermS (Mult x1 x2) =

solve :: TermP -> Maybe TermS
solve termP = beta (alpha (toTermS termP))

main :: IO ()
main = do
  s <- readLn
  print $ solve s
