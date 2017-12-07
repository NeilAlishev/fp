module Types where

-- Simply Typed Lambda Calculus
-- TAPL, ch. 9 & 11

type Symbol = String

data Term
  = Sym Symbol                  -- x
  | Lam Symbol Type Term        -- \(x : T) -> t
  | App Term Term               -- t1 t2
  --
  | Natural Int                 -- 5
  | Add Term Term               -- t1 + t2
  | Mult Term Term              -- t1 * t2
  --
  | Boolean Bool                -- True
  | Not Term                    -- not t
  | And Term Term               -- t1 `and` t2
  | Or Term Term                -- t1 `or` t2
  | Iff Term Term Term          -- if t1 then t2 else t3
  --
  | Pair Term Term              -- (t1, t2)
  | Fst Term                    -- fst t
  | Snd Term                    -- snd t
  --
  | Cons Term Term              -- t1 : t2
  | Nil                         -- []
  | IsNil Term                  -- null t
  | Head Term                   -- head t
  | Tail Term                   -- tail t
  deriving (Eq,Show,Read)

data Type
  = Base                        -- A
  | Fun Type Type               -- T1 -> T2
  | Nat                         -- N = {0,1,...}
  | Bool                        -- B = {False, True}
  | PairT Type Type             -- (T1, T2)
  | List Type                   -- [T]
  deriving (Eq,Show,Read)

