module Part1.Tasks where

import Util(notImplementedYet)
import Data.Fixed (mod')
import Data.List (sort)

factorial :: Integer -> Integer
factorial n = product [1 .. n]

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sinTaylor (normalizeAngle x) 20
  where
    normalizeAngle :: Double -> Double
    normalizeAngle angle = angle - 2 * pi * fromIntegral (round (angle / (2 * pi)))
    
    sinTaylor :: Double -> Int -> Double
    sinTaylor x n = loop n x x 1
      where
        loop 0 _ sum _ = sum
        loop k term sum sign
            | k == n = loop (k-1) (term * x * x / ((2*k+1)*(2*k+2))) (sum - term) (-sign)
            | otherwise = loop (k-1) (term * x * x / ((2*k+1)*(2*k+2))) (sum + sign * term) (-sign)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = cosTaylor (normalizeAngle x) 20
  where
    normalizeAngle :: Double -> Double
    normalizeAngle angle = angle - 2 * pi * fromIntegral (round (angle / (2 * pi)))
    
    cosTaylor :: Double -> Int -> Double
    cosTaylor x n = loop n 1 1 1
      where
        loop 0 _ sum _ = sum
        loop k term sum sign
            | k == n = loop (k-1) (term * x * x / ((2*k)*(2*k-1))) (sum - term) (-sign)
            | otherwise = loop (k-1) (term * x * x / ((2*k)*(2*k-1))) (sum + sign * term) (-sign)

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD x y = gcd' (abs x) (abs y)
  where
    gcd' a 0 = a
    gcd' a b = gcd' b (a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = case () of
  _
    | year <= 0 -> False
    | month <= 0 -> False
    | 12 < month -> False
    | day <= 0 -> False
    | daysInMonth month (isLeap year) < day -> False
    | otherwise -> True
  where
    isLeap year =
      year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)
    daysInMonth month is_leap =
      case month of
        2 -> if is_leap then 29 else 28
        x | x `elem` [4, 6, 9, 11] -> 30
        _ -> 31

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x 0 = 1
myPow x n
    | n < 0 = error "cant do with negative"
    | otherwise = multiplyNTimes x n
  where
    multiplyNTimes :: Integer -> Integer -> Integer
    multiplyNTimes _ 0 = 1
    multiplyNTimes val times = val * multiplyNTimes val (times - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = x `elem` [1, 2] || not (any isDivisor [2 .. maxPossibleDivisor])
  where
    maxPossibleDivisor = ceiling (sqrt $ fromInteger x)
    isDivisor y = x `mod` y == 0

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points =
  abs (sum (fmap det pairs)) / 2
  where
    det ((x1, y1), (x2, y2)) = x1 * y2 - x2 * y1
    pairs = zip points (tail points ++ [head points])

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = case () of
  _
    | a <= 0 || b <= 0 || c <= 0 -> bad
    | a + b <= c -> bad
    | b + c <= a -> bad
    | c + a <= b -> bad
    | otherwise -> case sort [a, b, c] of
        [a, b, c] | c ^ 2 == a ^ 2 + b ^ 2 -> straight
        [a, b, c] | c ^ 2 <= a ^ 2 + b ^ 2 -> sharp
        [a, b, c] | c ^ 2 >= a ^ 2 + b ^ 2 -> blunt
  where
    bad = -1
    blunt = 0
    sharp = 1
    straight = 2
