module Scripts where

import Data.Char
import Data.Maybe
import Data.List
import Matrices

--Scripts necessários para implementar AES

--converte inteiro em binario
int2bin :: Int -> Int
int2bin n = if n `div` 2 == 0 then n
            else n `rem` 2 + 10*int2bin (n `div` 2)

bin2int :: Int -> Int
bin2int n = if n `div` 10 == 0 then n
            else n `rem` 10 + 2*bin2int (n`div` 10)

--transforma inteiro em vetor com seus dígitos
digitos :: Int -> [Int]
digitos n | n `div` 10 == 0   = [n]
          | otherwise         = digitos (n `div` 10) ++ [n `rem`10]

--converte char em binario usando tabela asc 2
--no formato lista com 8 dígitos
char2bin :: Char -> [Int]
char2bin c = [0 | _<-[length dig..7]] ++ dig
    where dig = (digitos.int2bin.ord) c

bin2char :: [Int] -> Char
bin2char xs = chr $ bin2int $ bin xs
    where bin [] = 0
          bin xs = last xs + 10*(bin $ init xs)

--conversão bool - inteiro
bool2int :: Bool -> Int
bool2int True = 1
bool2int False = 0

int2bool :: Int -> Bool
int2bool 1 = True
int2bool 0 = False

--deslocamento à esquerda de uma lista
desloc :: [a] -> [a]
desloc xs = (tail xs) ++ [head xs]

--soma de polinômios em GF(2⁸) == subtração == ou exclusivo.
--(coeficientes dos polinomios estão em Z_2)
somaP :: [Int] -> [Int] -> [Int]
somaP p q = zipWith exOr p q

--faz p*x mod (x⁸+x⁴+x³+x+1)
--considera vetor de 9 posições, onde o mais à esquerda é o coeficiente do x⁸S
deslocMod :: [Int] -> [Int]
deslocMod p = if head (desloc p) == 0 then desloc p
              else somaP (desloc p) [1,0,0,0,1,1,0,1,1]

--converte polinomio em lista dos indices dos elem. não nulos [0,1,0,1,1,0,1,1,0]->[7,5,4,2,1]
p2in :: [Int] -> [Int]
p2in p = elemIndices 1 $ reverse p

--multiplicação de polinômios em GF(2⁸)
--ideia do algoritmo em http://www.cs.utsa.edu/~wagner/laws/FFM.html (improved multiplication)
multP :: [Int] -> [Int] -> [Int]
multP p q = tail $ foldl somaP [0,0,0,0,0,0,0,0,0] 
                  [a | (a,i) <- zip vetParcialResults [0..7], elem i $ p2in q]
    where vetParcialResults = (0:p) : map deslocMod vetParcialResults

--gera GF(256) a partir de um polinomio primitivo (no caso x+1 = [0,0,0,0,0,0,1,1])
--tabela de exponenciais de x+1
gf256 :: [[Int]]
gf256 = [0,0,0,0,0,0,0,0] : take 255 gf256'
    where gf256' = g : map (multP g) gf256'
          g = [0,0,0,0,0,0,1,1]

--inverso de um elemento no corpo GF(256)
inverso :: [Int] -> [Int]
inverso [0,0,0,0,0,0,0,1] = [0,0,0,0,0,0,0,1]
inverso x = gf256 !! (255 - (fromJust $ elemIndex x gf256))

--Rijndael S-box: modifica byte de forma não linear
--usado no subBytes
sBox :: [Int] -> [Int]
sBox x = concat $ somaM v $ multM m $ transpose [inverso x]
    where m = [[1,0,0,0,1,1,1,1],
               [1,1,0,0,0,1,1,1],
               [1,1,1,0,0,0,1,1],
               [1,1,1,1,0,0,0,1],
               [1,1,1,1,1,0,0,0],
               [0,1,1,1,1,1,0,0],
               [0,0,1,1,1,1,1,0],
               [0,0,0,1,1,1,1,1]]
          v = [[1],[1],[0],[0],[0],[1],[1],[0]]

--usados no mixColums:
--produto interno entre vetores de polinomios em GF(256)
prodInternoP :: [[Int]] -> [[Int]] -> [Int]
prodInternoP u v = foldl somaP [0,0,0,0,0,0,0,0] (zipWith (multP) u v)

-- multiplicação de matrizes de polinomios em GF(256)
multMP :: [[[Int]]] -> [[[Int]]] -> [[[Int]]]
multMP m1 m2 | l1 /= l2  = error "matrizes incompatíveis"
            | otherwise = [[prodInternoP l c | c<-transpose m2] | l<-m1]
            where l1 = snd $ lengthM m1
                  l2 = fst $ lengthM m2

--multiplica coluna de bytes por matriz MDS fixa
--usado no mixColumns
multMDS :: [[[Int]]] -> [[[Int]]]
multMDS x = multMP m x
    where m = [v,
               (desloc.desloc.desloc) v, 
               (desloc.desloc) v,
                desloc v]
        --v = [02, 03, 01, 01] em bytes
          v = [[0,0,0,0,0,0,1,0], [0,0,0,0,0,0,1,1], [0,0,0,0,0,0,0,1], [0,0,0,0,0,0,0,1]]


--usados na chave (Rijndael key schedule) :
--ideia em https://en.wikipedia.org/wiki/Rijndael_key_schedule

--exponenciação de 2 à i-1 (em GF(2⁸))
rcon :: Int -> [Int]
rcon 1 = [0,0,0,0,0,0,0,1]
rcon i = multP [0,0,0,0,0,0,1,0] $ rcon (i-1)

--key schedule core
--4 bytes de entrada e um índice i; 4 bytes de saída
ksc :: Int -> [[Int]] -> [[Int]]
ksc i xs = zipWith exOr (rcon i) (head v) : tail v
    where v = map sBox $ desloc xs

--take no final
takeLast :: Int -> [a] -> [a]
takeLast i = (reverse.(take i).reverse)

--drop no final
dropLast :: Int -> [a] -> [a]
dropLast i = (reverse.(drop i).reverse)


--usado na expansão da chave
--dado lista com bloco de 16 bytes da chave, gera os proximos 16. (i é  a rodada atual)
next16 :: (Int,[[Int]]) -> (Int,[[Int]])
next16 (i,bs) = (i+1,takeLast 4 c1 ++c2++c3++c4)
    where next4 cs = zipWith somaP (takeLast 4 $ dropLast 12 cs) $ takeLast 4 cs 
          c1 = bs ++ zipWith somaP (take 4 bs) (ksc i $ takeLast 4 bs)
          c2 = next4 c1
          c3 = next4 (c1++c2)
          c4 = next4 (c1++c2++c3)

--expansão da chave de 16 bytes (128 bits) para 176 bytes
{-transforma lista com 16 bytes da chave em lista com
11 matrizes de bytes 4x4 que contém a chave de cada rodada do algoritmo-}
keyExp :: [[Int]] -> [[[[Int]]]]
keyExp k = map (transpose.toMatrix.snd) $ take 11 v
    where v = (1,k) : map next16 v

--transforma lista de 16 itens em matriz 4x4
--4 primeiros itens formam primeira linha
toMatrix :: [a] -> [[a]]
toMatrix [] = []
toMatrix xs = (take 4 xs) : (toMatrix $ drop 4 xs)
          






