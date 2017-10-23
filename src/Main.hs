module Main where

newtype Symbol = Symbol { unSymbol :: String } deriving (Eq,Show,Read)

-- (1)
data TermS = SymS Symbol        -- x
           | LamS Symbol TermS  -- \x -> t
           | AppS TermS TermS   -- t1 t2
           deriving (Eq,Show,Read) -- ПОСМОТРЕТЬ КАК РАБОТАЕТ READ с коммандной строки

-- (1)
-- переименовать все переменные так, чтобы все они были разными.
alpha :: TermS -> TermS
alpha = error "Implement me!"

-- (1)
-- один шаг редукции, если это возможно. Стратегия вычислений - полная, т.е. редуцируются все возможные редексы.
beta :: TermS -> Maybe TermS

beta termS = do
    let reduced_term = _beta termS

    -- no reduction is possible
    if termS == reduced_term
        then Nothing
    else
        Just reduced_term

_beta (SymS sym) = SymS sym
_beta (LamS sym term) = LamS sym (_beta term)
_beta (AppS term1 term2) = _reduce term1 term2

-- we can't reduce this
_reduce (AppS term1 term2) (LamS sym term) = AppS (_reduce term1 term2) (LamS sym term) -- here print current AppS
_reduce (LamS sym term) (AppS term1 term2) = AppS (LamS sym term) (_reduce term1 term2) -- here print current AppS

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
    LamS current_sym (_apply_sym term (SymS sym) (LamS sym_arg term_arg))
-- traverse inside
_apply_lam (AppS term1 term2) (SymS sym) (LamS sym_arg term_arg) =
    AppS (_apply_sym term1 (SymS sym) (LamS sym_arg term_arg)) (_apply_sym term2 (SymS sym) (LamS sym_arg term_arg))

-- let
sym x = SymS (Symbol x)
lam x t = LamS (Symbol x) t
app t1 t2 = AppS t1 t2

-- Should be equal to Just $ lam "x" $ app (lam "f" $ sym "x") (lam "z" $ sym "z")
test_term1 = lam "x" $ app (app (lam "t" $ lam "f" $ sym "t") (sym "x")) (lam "z" $ sym "z")

test_term2 = lam "x" $ app (lam "f" $ sym "x") (lam "z" $ sym "z")
test_term3 = app (lam "x" $ sym "s") (sym "f")
test_term4 = (lam "x" $ sym "x")

data TermP = TermP TermS
           -- (4)
           | Natural Int
           | Plus TermP TermP
           | Mult TermP TermP
           -- (4*) 10%
           | Minus TermP TermP
           | Divide TermP TermP
           -- (5*) 50%
           | Y TermP
           -- (5**) 50%
           -- mutually recursive
           -- (7)
           | Cons TermP TermP
           | IsNil TermP
           | Head TermP
           | Tail TermP
           deriving (Eq,Show,Read)

toTermS :: TermP -> TermS
toTermS = error "Implement me!"

solve :: TermP -> Maybe TermS
solve termP = beta (alpha (toTermS termP))

main :: IO ()
main = do
  s <- readLn
  print $ solve s
