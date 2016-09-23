-- | the visitor pattern for Expr

module Data.Expr.Proposition.Visit where
import Data.Expr.Proposition.Types

-- ----------------------------------------

data Visitor r
  = V { vLit    :: Bool  -> r
      , vVar    :: Ident -> r
      , vUnary  :: Op1   -> r -> r
      , vBinary :: Op2   -> r -> r -> r
      }

idExpr :: Visitor Expr
idExpr
  = V Lit Var Unary Binary

visit :: Visitor r -> Expr -> r
visit v (Lit b) = vLit v b
visit v (Var name) = vVar v name
visit v (Unary op e) = (vUnary v) op $ visit v e
visit v (Binary op expr1 expr2) = (vBinary v) op (visit v expr1) (visit v expr2)



-- ----------------------------------------
