{-# LANGUAGE FlexibleInstances #-}

-- Week 5
module Calc where

import qualified Data.Map as M
import qualified ExprT
import qualified Parser
import qualified StackVM (Program, StackExp(..))


-- Ex. 1
eval :: ExprT.ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b


-- Ex. 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . Parser.parseExp ExprT.Lit ExprT.Add ExprT.Mul


-- Ex. 3
class Expr a
  where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT.ExprT
  where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul


-- Ex. 4
instance Expr Integer
  where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool
  where
    lit = (> 0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax
  where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax $ max a b
    mul (MinMax a) (MinMax b) = MinMax $ min a b

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7
  where
    lit = Mod7 . (`mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7


-- Ex. 5
instance Expr StackVM.Program
  where
    lit = (: []) . StackVM.PushI
    add = ((++ [StackVM.Add]) .) . (++)
    mul = ((++ [StackVM.Mul]) .) . (++)

compile :: String -> Maybe StackVM.Program
compile = Parser.parseExp lit add mul


-- Ex. 6
class HasVars a
  where
    var :: String -> a

data VarExprT
    = Lit Integer
    | Add VarExprT VarExprT
    | Mul VarExprT VarExprT
    | Var String
    deriving (Eq, Show)

instance Expr VarExprT
  where
    lit = Lit
    add = Add
    mul = Mul

instance HasVars VarExprT
  where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer)
  where
    var = flip (M.!?)

instance Expr (M.Map String Integer -> Maybe Integer)
  where
    lit = const . Just
    add a b vars = (+) <$> a vars <*> b vars
    mul a b vars = (*) <$> a vars <*> b vars
