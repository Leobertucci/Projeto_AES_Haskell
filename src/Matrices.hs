module Matrices where

import Data.List

--funções para operar com matrizes

type Matrix = [[Int]]

lengthM :: [[a]] -> (Int, Int)
lengthM mtx = (length mtx, length $ head mtx)

multEscalar :: Int -> Matrix -> Matrix
multEscalar a m = map (map (*a)) m

--transpose já definida em Data.List

--OU exclusivo para bits
exOr :: Int -> Int -> Int
exOr 0 b = b
exOr 1 b = 1-b

--soma com OU exclusivo pois trabalharemos em Z_2
somaM :: Matrix -> Matrix -> Matrix
somaM m1 m2 = zipWith (zipWith (exOr)) m1 m2

--produto interno em Z_2
prodInterno :: [Int] -> [Int] -> Int
prodInterno u v = foldl exOr 0 (zipWith (*) u v)  

--retorna a linha i
row :: Int -> [[a]] -> [[a]]
row i m = [m !! i]

--retorna a coluna i
col :: Int -> [[a]] -> [[a]]
col i m = transpose [(transpose m) !! i]

multM :: Matrix -> Matrix -> Matrix
multM m1 m2 | l1 /= l2  = error "matrizes incompatíveis"
            | otherwise = [[prodInterno l c | c<-transpose m2] | l<-m1]
            where l1 = snd $ lengthM m1
                  l2 = fst $ lengthM m2





