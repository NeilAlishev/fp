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
typeOf term = _typeOf (alpha term)

_typeOf :: Term -> Either String Type
_typeOf converted_term = Left "TODO"

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

