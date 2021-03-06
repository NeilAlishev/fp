module Solution where

import Data.List

import Types
import PreviousSem

data ContextElem = ContextElem { var :: String, varType :: Type } deriving (Eq, Show, Read)
type Context = [ContextElem]

addContextElem :: Context -> String -> Type -> Context
addContextElem ctx var varType = (ContextElem var varType) : ctx

getTypeFromContext :: Context -> String -> Maybe Type
getTypeFromContext ctx varName = do
    ctxElem <- find (\(ContextElem var _) -> var == varName) ctx
    return (varType ctxElem) -- return automatically wraps (varType ctxElem) in Just

typeOf :: Term -> Either String Type

-- perform alpha conversion to handle cases like Lam "x" Base (Lam "x" Base (Sym "x"))
typeOf term = _typeOf (alpha term) []

_typeOf :: Term -> Context -> Either String Type

-- BASIC LAMBDA CALCULUS TYPES

_typeOf (Sym x) ctx =
    case getTypeFromContext ctx x of
        Just var_type -> Right var_type
        Nothing -> Left ("Variable " ++ x ++ " is of unknown type")

_typeOf (Lam sym sym_type term) ctx =
    let updatedContext = addContextElem ctx sym sym_type
    in case _typeOf term updatedContext of
        Right var_type -> Right (Fun sym_type var_type)
        Left error -> Left error

_typeOf (App term1 term2) ctx =
    case term1Type of
        Right (Fun t1 t2) ->
            case term2Type of
                Right t3 ->
                    if t1 == t3
                        then Right t2
                    else
                        Left "App type mismatch between parameter and argument"
                Left error -> Left error
            where term2Type = _typeOf term2 ctx

        Right _ -> Left "Fun type expected"
        Left error -> Left error
    where term1Type = _typeOf term1 ctx

-- BOOLEAN TYPES

_typeOf (Boolean bool) ctx = Right Bool

_typeOf (Not term) ctx =
    case termType of
        Right Bool -> Right Bool
        Right _ -> Left "Not a Bool type inside Not"
        Left error -> Left error
    where termType = _typeOf term ctx

_typeOf (And term1 term2) ctx =
    case term1Type of
        Right Bool ->
            case term2Type of
                Right Bool -> Right Bool
                Right _ -> Left "Not a Bool type inside And"
                Left error -> Left error
            where term2Type = _typeOf term2 ctx
        Right _ -> Left "Not a Bool type inside And"
        Left error -> Left error
    where term1Type = _typeOf term1 ctx

_typeOf (Or term1 term2) ctx =
    case term1Type of
        Right Bool ->
            case term2Type of
                Right Bool -> Right Bool
                Right _ -> Left "Not a Bool type inside Or"
                Left error -> Left error
            where term2Type = _typeOf term2 ctx
        Right _ -> Left "Not a Bool type inside Or"
        Left error -> Left error
    where term1Type = _typeOf term1 ctx

_typeOf (Iff term1 term2 term3) ctx =
    case term1Type of
        Right Bool ->
            case term2Type of
                Right t1 ->
                    case term3Type of
                        Right t2 ->
                            if t1 == t2
                                then Right t1
                            else
                                Left "If branches evaluate to different types"

                        Left error -> Left error
                    where term3Type = _typeOf term3 ctx

                Left error -> Left error
            where term2Type = _typeOf term2 ctx

        Right _ -> Left "Not a Bool type inside If condition"
        Left error -> Left error
    where term1Type = _typeOf term1 ctx

-- NUMERIC TYPES

_typeOf (Natural n) ctx
    | n > 0 = Right Nat
    | otherwise = Left "Natural number should be greater than 0"

_typeOf (Add term1 term2) ctx =
    case term1Type of
        Right Nat ->
            case term2Type of
                Right Nat -> Right Nat
                Right _ -> Left "Not a Nat type inside Add"
                Left error -> Left error
            where term2Type = _typeOf term2 ctx
        Right _ -> Left "Not a Nat type inside Add"
        Left error -> Left error
    where term1Type = _typeOf term1 ctx

_typeOf (Mult term1 term2) ctx =
    case term1Type of
        Right Nat ->
            case term2Type of
                Right Nat -> Right Nat
                Right _ -> Left "Not a Nat type inside Mult"
                Left error -> Left error
            where term2Type = _typeOf term2 ctx
        Right _ -> Left "Not a Nat type inside Mult"
        Left error -> Left error
    where term1Type = _typeOf term1 ctx

-- PAIR TYPE

_typeOf (Pair term1 term2) ctx =
    case term1Type of
        Right t1 ->
            case term2Type of
                Right t2 -> Right (PairT t1 t2)
                Left error -> Left error
            where term2Type = _typeOf term2 ctx
        Left error -> Left error
    where term1Type = _typeOf term1 ctx
    -- check if term1 is correct, term2 is correct, return PairT with two types

_typeOf (Fst term) ctx =
    case termType of
        Right (PairT t1 t2) -> Right t1
        Right _ -> Left "Term in Fst should be a Pair"
        Left error -> Left error
    where termType = _typeOf term ctx
    -- check if term is a pair, then return type of the first argument in pair

_typeOf (Snd term) ctx =
    case termType of
        Right (PairT t1 t2) -> Right t2
        Right _ -> Left "Term in Snd should be a Pair"
        Left error -> Left error
    where termType = _typeOf term ctx

-- LIST TYPE

-- 1 : [2,3] = [1,2,3]
_typeOf (Cons term1 term2) ctx =
    case term1Type of
        Right t1 ->
            case term2Type of
                Right (List t2) ->
                    if t1 == t2
                        then Right (List t2)
                    else
                        Left "Element and List in Cons should be of the same type"
                Right _ -> Left "Second argument in Cons should be a List"
                Left error -> Left error
            where term2Type = _typeOf term2 ctx
        Left error -> Left error
    where term1Type = _typeOf term1 ctx

_typeOf (Nil listType) ctx =
    Right (List listType)

_typeOf (IsNil term) ctx =
    case termType of
        Right (List _) -> Right Bool
        Right _ -> Left "Term in isNil should be a List"
        Left error -> Left error
    where termType = _typeOf term ctx

_typeOf (Head term) ctx =
    case termType of
        Right (List t1) -> Right t1
        Right _ -> Left "Term in Head should be a List"
        Left error -> Left error
    where termType = _typeOf term ctx

_typeOf (Tail term) ctx =
    case termType of
        Right (List t1) -> Right (List t1)
        Right _ -> Left "Term in Tail should be a List"
        Left error -> Left error
    where termType = _typeOf term ctx

-- > typeOf $ Lam "x" $ Add (Sym "x") (Natural 5)
-- Right (Fun Nat Nat)

-- > typeOf $ Lam "x" $ Sym "x"
-- Right (Fun A A)

-- > typeOf $ Add (Natural 5) (Boolean False)
-- Left "..."

-- > typeOf $ App (Lam "x" $ Sym "x") (Natural 5)
-- Right Nat

-- > typeOf $ App (Lam "x" $ Boolean False) (Natural 5)
-- Right Bool

