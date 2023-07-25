--Lista 2B

import Data.Char

--Ex 1
quadrado = [x^2 | x <- [1 .. 100]]


--Ex 2
replica n x = [x | _ <- [1 ..n]]


--Ex 3
pyts :: Int -> [(Int, Int, Int)]
pyts n = [(x, y, z) | x <-[1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]


--Ex 4
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n-1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], x == sum(factors x)]

----sum = soma elementos de uma lista 
------ex: sum [1,2,3,4] = 10


--Ex 5
concatenar = concat [[(x, y) | y <- [4,5,6]] | x <- [1,2,3]]


--Ex 6
find :: Eq elemento => elemento -> [(elemento, posicao)] -> [posicao]
find elemento listaDtuplas = [posicao | (x, posicao) <- listaDtuplas, x == elemento]

positions :: Eq a => a -> [a] -> [Int] 
positions x xs = find x (zip xs [0..n]) 
	where n = (length xs) - 1	
----Eq a = é um filtro que diz que a tem que ser um tipo de dado comparavel.

----zip = combina os elementos de duas listas, criando um par com o primeiro elemento da primeira e o primeiro elemento da segunda
------ex: zip [1,2,3] ['a','b'] = [(1,'a'), (2,'b')]


--Ex 7
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct list1 list2 = sum [x * y | (x,y)<- zip list1 list2]


--Ex 8
(&!) :: Int -> Int -> Int
_ &! 0 = 1
x &! n = x * ( x &! (n-1))


--Ex 9
mapfilter f p xs = map f (filter p xs)


--Ex 10
dec2Int :: [Int] -> Int
dec2Int    [] = 0
dec2Int    (x:cauda) = x * 10^(length cauda) + dec2Int cauda

--Ex 11



---------------
--Aula Gravada List Compreension

func04 :: String -> [Int]
func04    l = [ord a | a<- l, (ord 'a') <= ord a , ord a <= (ord 'z') ]
--função que retorna a posição das letras no alfabeto


maior :: Int -> Int -> Int
maior a      b
	|a > b = a
	|otherwise = b
	
func05 :: [(Int,Int)] -> [Int]
func05    l = [maior a b | (a,b) <- l]

func06 :: [(Int,Int)] -> [Int]
func06    l = [maior (fst a) (snd a) | a <- l]

func07 :: [(Bool,Int)] -> [Int]
func07    l = [b | (a,b) <- l, a == True]

aux8 :: (Bool,[Int]) -> [Int]
aux8    (a,b)
	|a = [x | x <- b , mod x 2 /= 0]
	|otherwise = b

func08 :: [(Bool,[Int])] -> [[Int]]
func08    l = [ aux8 a | a <- l]

-----------
--generica

func2gg  :: ((t, u) -> z) -> [(t,u)] -> [z]
func2gg f  l  = [f x | x <- l]

isasc :: (Int,Char) -> Bool
isasc (a,b)
	|a == ord b = True
	|otherwise = False
	
maior2 :: (Int,Int) -> Int
maior2 (a,b)
	|a > b = a
	|otherwise = b
	
--Prova 2

func1 :: String -> Int
func1 (h:t)
	|h == '1'||h == '2'||h == '3' || h == '4' || h == '5'= 1 + (func1 t)
	|otherwise = 0 +  func1 t
func1 [] = 0

func2 :: String -> String -> String
func2   [] a = a
func2   a [] = a
func2   a  b 
	|(func1 a) > (func1 b) = a
	|otherwise = b

func3 :: (String -> String -> String)->[(String,String)]->[String]
func3 f l = [f a b | (a,b) <- l]