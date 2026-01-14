{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
  mConstruct :: Int -> Int -> (Int -> Int -> Int) -> mx
  mWidth :: mx -> Int
  mHeight :: mx -> Int
  mAt :: mx -> Int -> Int -> Int


-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
  mConstruct _ _ f = f 0 0
  mWidth _ = 1
  mHeight _ = 1
  mAt a _ _ = a

instance Matrix [[Int]] where
  mConstruct w h f = [[ f x y | x <- [0..w-1]] | y <- [0..h-1]]
  mWidth = length . head
  mHeight = length
  mAt m x y = (m !! y) !! x 

instance Matrix (SparseMatrix Int) where
  mConstruct w h f = SparseMatrix w h elements
    where 
      elements = fromList [((x, y), v) | x <- [0..w-1], y <- [0..h-1], let v = f x y, v /= 0]
  mWidth = sparseMatrixWidth
  mHeight = sparseMatrixHeight
  mAt m x y = fromMaybe 0 (sparseMatrixElements m !? (x, y))


-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = mConstruct w w (\ x y -> if x == y then 1 else 0)

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = mConstruct w h (const $ const 0)

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix lhs rhs= mConstruct (mWidth rhs) (mHeight lhs) (\ x y -> sum [mAt lhs k y * mAt rhs x k | k <- [0 .. (mWidth lhs - 1)]])

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant mx = sum [ permutationSign permutation * selectByPermutation permutation | permutation <- permutations [0..n-1]]
  where 
    n = mHeight mx
    inversions permutation = length [(i, j) | i <- [0..n-2], j <- [i+1..n-1], permutation !! i > permutation !! j]
    permutationSign permutation = if inversions permutation `mod` 2 == 0 then 1 else -1
    selectByPermutation permutation = Prelude.foldr (\ (i, pi) acc -> acc * mAt mx i pi) 1 $ zip [0..n-1] permutation
