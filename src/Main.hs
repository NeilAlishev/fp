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
           deriving (Eq,Read) -- ПОСМОТРЕТЬ КАК РАБОТАЕТ READ с коммандной строки, TEMPORARY DELETED SHOW

-- TEMPORARY SHOW - for debugging
instance (Show TermS) where
    show (SymS x) = sq (show (unSymbol x))
    show (LamS sym term) = "λ" ++ sq (show (unSymbol sym)) ++ "." ++ (show term)
    show (AppS term1 term2) = (show term1) ++ " " ++ (show term2)

-- remove double quotes
sq :: String -> String
sq s@[c]                     = s
sq ('"':s)  | last s == '"'  = init s
            | otherwise      = s
sq ('\'':s) | last s == '\'' = init s
            | otherwise      = s
sq s                         = s

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
_reduce (AppS term1 term2) (SymS sym) = AppS (_reduce term1 term2) (SymS sym)
_reduce (SymS sym) (AppS term1 term2) = AppS (SymS sym) (_reduce term1 term2) -- this should go to _reduce Sym Sym
_reduce (AppS first_term1 first_term2) (AppS second_term1 second_term2) =
    -- always reduce first application
    AppS (_reduce first_term1 first_term2) (AppS second_term1 second_term2)

-- can't reduce this, leave untouched
_reduce (SymS s1) (SymS s2) = AppS (SymS s1) (SymS s2)
_reduce (SymS s1) (LamS sym term) = AppS (SymS s1) (LamS sym term)

-- we can reduce this
_reduce (LamS sym term) (SymS arg) = _apply_sym term (SymS sym) (SymS arg)
_reduce (LamS sym term) (LamS sym_arg term_arg) = _apply_lam term (SymS sym) (LamS sym_arg term_arg)
_reduce (LamS sym term) (AppS term1 term2) = _apply_app term (SymS sym) (AppS term1 term2)

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

_apply_app :: TermS -> TermS -> TermS -> TermS
_apply_app (SymS current_sym) (SymS sym) (AppS term1 term2) =
    -- apply app
    if current_sym == sym
        then AppS term1 term2
    else
        SymS current_sym

-- traverse inside
_apply_app (LamS current_sym term) (SymS sym) (AppS term_arg1 term_arg2) =
    LamS current_sym (_apply_app term (SymS sym) (AppS term_arg1 term_arg2))
-- traverse inside
_apply_app (AppS term1 term2) (SymS sym) (AppS term_arg1 term_arg2) =
    AppS (_apply_app term1 (SymS sym) (AppS term_arg1 term_arg2)) (_apply_app term2 (SymS sym) (AppS term_arg1 term_arg2))

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
           | Boolean Bool
           | Pair TermP TermP
           | Fst TermP
           | Snd TermP
           | Cons TermP TermP
           | Nil -- для пустого списка
           | IsNil TermP
           | Head TermP
           | Tail TermP
           deriving (Eq,Show,Read)

-- we can perform arithmetic operations only on Church numerals, not on arbitrary lambda expressions
test_term6 = Plus (Natural 2) (Natural 2)
test_term7 = Mult (Natural 3) (Natural 3)

zero = lam "s" $ lam "z" $ sym "z"
scc = lam "n" $ lam "s" $ lam "z" $ app (sym "s") (app (app (sym "n") (sym "s")) (sym "z"))
plus = lam "x" $ lam "y" $ lam "s" $ lam "z" $ app (app (sym "x") (sym "s")) (app (app (sym "y") (sym "s")) (sym "z"))
times = lam "x" $ lam "y" $ lam "s" $ lam "z" $ app (app (sym "x") (app (sym "y") (sym "s"))) (sym "z")

-- testing Church numerals
one = lam "s" $ lam "z" $ app (app (app scc zero) (sym "s")) (sym "z")
two = lam "s" $ lam "z" $ app (app (app scc one) (sym "s")) (sym "z")

-- list implementation

pair' = lam "a" $ lam "b" $ lam "f" $ app (app (sym "f") (sym "a")) (sym "b")
fst'  = lam "p" $ app (sym "p") (lam "a" $ lam "b" $ sym "a")
snd'  = lam "p" $ app (sym "p") (lam "a" $ lam "b" $ sym "b")

tru = lam "frst" $ lam "scnd" $ sym "frst"
fls = lam "frst" $ lam "scnd" $ sym "scnd"

nil' = Pair (Boolean True) (Boolean True)
cons' h t = toTermS (Pair (Boolean False) (Pair h t))
isNil' t = Fst t
head' z = Fst (Snd z)
tail' z = Snd (Snd z)

toTermS :: TermP -> TermS

-- convert natural number to the Church numeral
toTermS (Natural x) = full_beta (_toTermS 0 x zero)

-- arithmetic operations
toTermS (Plus x1 x2) = lam "s" $ lam "z" $ full_beta (app (app (app (app plus (toTermS x1)) (toTermS x2)) (sym "s")) (sym "z"))
toTermS (Mult x1 x2) = lam "s" $ lam "z" $ full_beta (app (app (app (app times (toTermS x1)) (toTermS x2)) (sym "s")) (sym "z"))

-- implement pair
toTermS (Pair term1 term2) = full_beta (app (app pair' (toTermS term1)) (toTermS term2))
toTermS (Fst term) = full_beta (app fst' (alpha (toTermS term)))
toTermS (Snd term) = full_beta (app snd' (alpha (toTermS term)))

-- implement simple Boolean
toTermS (Boolean val) =
  if val
    then tru
  else
    fls

-- implement list
toTermS Nil = full_beta (toTermS nil')
toTermS (Cons term1 term2) = cons' term1 term2
toTermS (IsNil term) = toTermS (isNil' $ term)
toTermS (Head term) = toTermS (head' $ term)
toTermS (Tail term) = toTermS (tail' $ term)

_toTermS current_num target_num current_church_num
  | current_num == target_num = current_church_num
  | current_num < target_num =
    let next_church_num = lam "s" $ lam "z" $ app (app (app scc current_church_num) (sym "s")) (sym "z")
    in _toTermS (current_num + 1) target_num next_church_num

solve :: TermP -> Maybe TermS
solve termP = beta (alpha (toTermS termP))

main :: IO ()
main = do
  s <- readLn
  print $ solve s
