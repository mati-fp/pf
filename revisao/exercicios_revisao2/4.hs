-- Dado o seguinte tipo para expressões aritméticas que possuem variáveis de um tipo a:
-- data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)
-- a) Mostre como tornar o tipo Expr um membro das classes Functor, Aplicative e Monad.
-- b) Com a ajuda de um exemplo, explique qual o significado do operador (>>=) sobre esse tipo
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)
-- tornando Expr um membro da classe Functor
instance Functor Expr where
  fmap f (Var x) = Var (f x)
  fmap _ (Val x) = Val x
  fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)

-- tornando Expr um membro da classe Applicative
instance Applicative Expr where
  pure = Var
  (Var f) <*> e = fmap f e
  _ <*> _ = error "Applicative Expr: Val and Add cannot be used as functions"

-- tornando Expr um membro da classe Monad
instance Monad Expr where
  return = Var
  (Var x) >>= f = f x
  (Val x) >>= _ = Val x
  (Add e1 e2) >>= f = Add (e1 >>= f) (e2 >>= f)