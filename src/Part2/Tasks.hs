module Part2.Tasks where

import Util(notImplementedYet)
import Data.Semigroup (Min(Min))

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times
(|/|) :: Term -> Term -> Term
(|/|) = BinaryTerm Divide

infixl 6 |+|

infixl 6 |-|

infixl 7 |*|

infixl 7 |/|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement = go
  where
    go expr = case expr of
      Variable name | name == varName -> replacement
      Constant _ -> expr
      BinaryTerm op left right -> 
        BinaryTerm op (go left) (go right)
      UnaryTerm op inner ->
        UnaryTerm op (go inner)
      _ -> expr

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate term = case term of
  BinaryTerm op left right ->
    let lhs = evaluate left
        rhs = evaluate right
    in case (lhs, rhs) of
      (IntConstant l, IntConstant r) ->
        case op of
          Plus   -> IntConstant (l + r)
          Minus  -> IntConstant (l - r)
          Times  -> IntConstant (l * r)
          Divide -> 
            if r == 0 
              then error "error divide 0" 
              else IntConstant (l `div` r)
      _ -> BinaryTerm op lhs rhs
  
  _ -> term
