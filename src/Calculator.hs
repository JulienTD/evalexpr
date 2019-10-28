module Calculator where

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Lit Integer

          