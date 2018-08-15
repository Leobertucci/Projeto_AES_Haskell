module Main where

import Data.List
import Matrices
import Scripts

--funções para encriptação
subBytes :: [[[Int]]] -> [[[Int]]]
subBytes xss = map (map sBox) xss

shiftRows :: [[a]] -> [[a]]
shiftRows (l0:l1:l2:l3:[]) = l0 : desloc l1 : (desloc.desloc) l2 : [(desloc.desloc.desloc) l3]

mixColumns :: [[[Int]]] -> [[[Int]]]
mixColumns xss = transpose [head $ transpose $ multMDS $ col i xss | i <- [0..3]]

addRoundKey :: String -> Int -> [[[Int]]] -> [[[Int]]]
addRoundKey k i xss = zipWith (zipWith somaP) xss $ key !! i
    where key = keyExp $ map char2bin k

--funções para decriptação
subBytesInv :: [[[Int]]] -> [[[Int]]]
subBytesInv xss = map (map sBoxInv) xss

shiftRowsInv :: [[a]] -> [[a]]
shiftRowsInv (l0:l1:l2:l3:[]) = l0 : (desloc.desloc.desloc) l1 : (desloc.desloc) l2 : [desloc l3]

mixColumnsInv :: [[[Int]]] -> [[[Int]]]
mixColumnsInv xss = transpose [head $ transpose $ multMDSinv $ col i xss | i <- [0..3]]

--addRoundKeyInv :: String -> Int -> [[[Int]]] -> [[[Int]]]
--addRoundKeyInv k i xss = zipWith (zipWith somaP) xss $ key !! i
--    where key = keyExp $ map char2bin k

--algoritmo AES 128 bits
--recebe chave e bloco de 128 bits no formato de uma matriz e encripta
aes :: String -> [[[Int]]] -> [[[Int]]]
aes k b = ((addRoundKey k 10 ).shiftRows.subBytes.snd) $ last $ take 10 temp
    where temp = (0,addRoundKey k 0 b) : map prox temp
          prox :: (Int,[[[Int]]]) -> (Int,[[[Int]]])
          prox (i,b) = (i+1, ((addRoundKey k (i+1) ).mixColumns.shiftRows.subBytes) b)

--inverso AES 128 bits
aesInv :: String -> [[[Int]]] -> [[[Int]]]
aesInv k b = addRoundKey k 0 $ snd $ last $ take 10 temp
    where temp = (10,(subBytesInv.shiftRowsInv.(addRoundKey k 10)) b) : map prox temp
          prox :: (Int,[[[Int]]]) -> (Int,[[[Int]]])
          prox (i,b) = (i-1, (subBytesInv.shiftRowsInv.mixColumnsInv.(addRoundKey k (i-1))) b)

encode :: String -> String -> String
encode k m = map bin2char $ concat $ transpose $ aes k $ (transpose.toMatrix) $ map char2bin m

decode :: String -> String -> String
decode k c = map bin2char $ concat $ transpose $ aesInv k $ (transpose.toMatrix) $ map char2bin c

main :: IO ()
main = do
  let m = "hello world55556"
  let k = "1234567898765431"
  let c = encode k m
  let d = decode k c
  print ("Mensagem: " ++ m)
  print ("Codificado: " ++ c)
  print ("Decodificado: " ++ d)






