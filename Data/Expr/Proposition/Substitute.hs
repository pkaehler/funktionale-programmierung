module Data.Expr.Proposition.Substitute where

import Data.List      (union)

import Data.Expr.Proposition.Types
import Data.Expr.Proposition.Visit

-- ----------------------------------------
-- variable substitution

type VarEnv = [(Ident, Expr)]

substVars :: VarEnv -> Expr -> Expr

substVars env
    = visit $
        V {vLit   = Lit
        , vVar    = \x -> case lookup x env of
                          Nothing -> Var x
                          Just a -> a
        , vUnary  = Unary
        , vBinary = Binary
        }

freeVars :: Expr -> [Ident]
freeVars
    = visit $
      V {vLit   = \b -> []
      , vVar    = \i -> [i]
      , vUnary  = \op1 k1 -> k1
      , vBinary = \op2 k2 k3 -> k2 ++ k3
      }


-- ----------------------------------------
