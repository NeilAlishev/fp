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

