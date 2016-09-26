module Data.Expr.Proposition.Substitute where

import Data.List      (union)

import Data.Expr.Proposition.Types

-- ----------------------------------------
-- variable substitution

type VarEnv = [(Ident, Expr)]

substVars :: VarEnv -> Expr -> Expr
substVars _ x@(Lit _)         = x
substVars env (Var a)         = snd . head . filter ((==a) . fst) $ env
substVars env (Unary op a)    = Unary op (substVars env a)
substVars env (Binary op a b) = Binary op (substVars env a) (substVars env b)

freeVars :: Expr -> [Ident]
freeVars (Lit _)        = []
freeVars (Var a)        = [a]
freeVars (Unary _ a)    = freeVars a
freeVars (Binary _ a b) = union (freeVars a) (freeVars b)



-- ----------------------------------------
