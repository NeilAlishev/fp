module Solution where

import Types

typeOf :: Term -> Either String Type
typeOf = error "Implement me!"

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

