module Part3.Tasks where

import Data.Foldable (find, maximumBy)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Util (notImplementedYet)
import Data.Maybe (fromMaybe)
import Data.List (maximumBy)
import Data.List (find)
import qualified Data.Map as Map
import Data.Map (Map)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = fmap f [n ..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq xs = fst $ maximumBy (comparing snd) $ freq $ concatMap digits xs
  where
    digits :: Int -> [Int]
    digits n
        | n < 0     = digits (-n)
        | n < 10    = [n]
        | otherwise = n `mod` 10 : digits (n `div` 10)
    
    freq :: [Int] -> [(Int, Int)]
    freq = foldr increment []
    
    increment :: Int -> [(Int, Int)] -> [(Int, Int)]
    increment x [] = [(x, 1)]
    increment x ((y, count):rest)
        | x == y    = (y, count + 1) : rest
        | otherwise = (y, count) : increment x rest

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x : xs) = x : uniq (filter (/= x) xs)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: Ord k => (a -> k) -> [a] -> [(k, [a])]
grokBy f xs = Map.toList $ foldr insertItem Map.empty xs
  where
    insertItem x = Map.insertWith (++) (f x) [x]
