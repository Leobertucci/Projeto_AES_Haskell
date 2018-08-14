module Main where

import Data.List
import Matrices
import Scripts

subBytes :: [[[Int]]] -> [[[Int]]]
subBytes xss = map (map sBox) xss

shiftRows :: [[a]] -> [[a]]
shiftRows (l0:l1:l2:l3:[]) = l0 : desloc l1 : (desloc.desloc) l2 : [(desloc.desloc.desloc) l3]

mixColumns :: [[[Int]]] -> [[[Int]]]
mixColumns xss = transpose [head $ transpose $ multMDS $ col i xss | i <- [0..3]]

addRoundKey :: String -> Int -> [[[Int]]] -> [[[Int]]]
addRoundKey k i xss = zipWith (zipWith somaP) xss $ key !! i
    where key = keyExp $ map char2bin k


--algoritmo AES 128 bits
--recebe chave e bloco de 128 bits no formato de uma matriz e encripta
aes :: String -> [[[Int]]] -> [[[Int]]]
aes k b = ((addRoundKey k 10 ).shiftRows.subBytes.snd) $ last $ take 10 temp
    where temp = (0,addRoundKey k 0 b) : map prox temp
          prox :: (Int,[[[Int]]]) -> (Int,[[[Int]]])
          prox (i,b) = (i+1, ((addRoundKey k (i+1) ).mixColumns.shiftRows.subBytes) b)

encode :: String -> String -> String
encode k m = map bin2char $ concat $ transpose $ aes k $ (transpose.toMatrix) $ map char2bin m

main :: IO ()
main = do
  putStrLn "hello world55555"
  let c = encode "1234567898765431" "hello world55556"
  print ("Codificado: " ++ c)






