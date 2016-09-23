-- | the visitor pattern for Expr

module Data.Expr.Proposition.Visitors where

import Data.Expr.Proposition.Types
import Data.Expr.Proposition.Visit
import Data.Expr.Proposition.Eval (mf1, mf2)

import Data.Set(Set)
import qualified Data.Set as S

-- ----------------------------------------

type Idents = Set Ident

freeVars :: Expr -> Idents
freeVars
  = visit $
    V {vLit   = \b -> S.empty
    , vVar    = \i -> S.singleton i
    , vUnary  = \op1 k1 -> k1
    , vBinary = \op2 k2 k3 -> S.union k2 k3
    }

type VarEnv = [(Ident, Expr)]

substVars :: VarEnv -> Expr -> Expr
substVars env
  = visit $
  V {vLit   = Lit
  , vVar    = \i->   case lookup x  env of
                          Nothing -> Var x
                          just a -> Var a
  , vUnary  = Unary
  , vBinary = Binary
  }

eval :: Expr -> Bool
eval
  = visit $
  V {vLit   = Lit
  , vVar    = \i->   case lookup x  env of
                          Nothing -> Var x
                          just a -> Var a
  , vUnary  = Unary
  , vBinary = Binary
  }

-- ----------------------------------------
