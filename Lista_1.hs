import Data.Char (isUpper, toLower, toUpper, ord, isDigit)

--Lista 1

--Exercicio 1
funcao1 :: Double -> Double
funcao1     x
	|x >= 0 = (x+4)/(x+2)
	|otherwise = 2/x

funcao2 :: Int -> Int -> Int
funcao2    x      y
	|x >= y = x + y
	|otherwise = x - y

funcao3 :: Int -> Int -> Int -> Int
funcao3    x      y      z
	| (x+y) > z = x + y + z
	| (x+y) < z = x - y - z
	|otherwise = 0

--Exercicio 2
fat :: Int -> Int
fat    0 = 1
fat    x = x * fat(x-1)

--Exercicio 3

soma :: Int -> Int -> Int
soma    x      y    = x + y

multRec :: Int -> Int -> Int
multRec    _      0    =  0
multRec    x      y    =  soma x (multRec x (y-1))

--Exercicio 4
invertInt :: Int -> Int
invertInt    0    = 0
invertInt    x    
	| x > 0 = read(reverse(show x)) :: Int
	| x < 0 = - (read(reverse(show (-x))) :: Int)

--Exercicio 5
square :: Int -> Int
square    x    = x * x

fourPower :: Int -> Int
fourPower x = (square (square x))

fourPower2 :: Int -> Int
fourPower2 x 
	|x > 0 = y * y
	|x < 0 = -1
	where
		y = square x

--Exercicio 6
raizSeis :: Float -> Float
raizSeis    1      = sqrt(6)
raizSeis    x = sqrt(6 + raizSeis(x-1))

--Exercicio 7
possibEscolha :: Int -> Int -> Int
possibEscolha n      m
	| m < 0 || n < 0 || m < n = 0
	| m == 0 || n == 0 = 1
	|otherwise = possibEscolha (n-1) (m-1) * m`div`n


--Exercicio 18
qq :: Char -> Int
qq    x	
	|isDigit x = read [x]
	|otherwise = -1
	

qqqq :: Char -> Int
qqqq    x     
	|isDigit x      = ord x - ord '0'
	|otherwise      = -1




--Lista 2 A

--Exercicio 1

retChar :: Char -> (Char, Char, Int)
retChar    x    
	|isUpper(x) = (x, toLower(x), ord(x))
	|otherwise  = (x, toUpper(x),ord(x))


--Exercicio 2

type Dados = (String, Int, Char)

pessoa :: Int -> Dados
pessoa rg
    | rg == 1 = ("Joao Silva", 6, 'm')
    | rg == 2 = ("Jonas Souza", 5, 'm')
    | rg == 3 = ("Jocileide Strauss", 21, 'f')
	|rg  == 4 = ("aaa", 18, 'm')
    | otherwise = ("Nao ha ninguem mais", 0, 'x')


--a)
menorIdade :: Int -> String
menorIdade x
    | x > 0 && x < 9999 = encontraMenor 1 (pessoa 1) x
    | otherwise = ""

encontraMenor :: Int -> Dados -> Int -> String
encontraMenor    rg menorAtual maxRg
    | rg <= maxRg = if idadeAtual < idadeMenor then encontraMenor (rg + 1) pessoaAtual maxRg else encontraMenor (rg + 1) menorAtual maxRg
    | otherwise = nomeMenor
    where
        (nomeMenor, idadeMenor, _) = menorAtual
        pessoaAtual = pessoa rg
        (_, idadeAtual, _) = pessoaAtual

--b)
idadeMedia :: Int -> String
idadeMedia    rg
	|rg > 0 && rg < 9999 = show(fromIntegral(somaIdades 1 rg) / fromIntegral rg)
	|otherwise = ""

somaIdades :: Int -> Int -> Int 
somaIdades    rg     maxRg  
	|rg <= maxRg = idadeAt + somaIdades (rg+1) maxRg
	|otherwise = 0
	where	
		(_,idadeAt,_) = pessoaAt
		pessoaAt = pessoa rg
		
--c)

numeroH :: Int -> Int
numeroH    rg
	|rg > 0 && rg < 9999 = contaH 1 rg
	|otherwise = 0
	
contaH :: Int -> Int -> Int
contaH    rg     maxRg
	|rg <= maxRg && sexo == 'm' = 1 + contaH (rg+1) maxRg
	|rg <= maxRg && sexo /= 'm' = contaH (rg+1) maxRg
	|otherwise = 0
	where
		(_,_,sexo) = pessoa rg
		--pessoaAt = pessoa rg
		
--d)

maiorIdade :: Int -> Int
maiorIdade    rg     
	|rg > 0 && rg < 9999 = if (encontraMaior rg (pessoa rg) 1) == idadeRg then rg else maiorIdade (rg - 1) 
	|rg == 0 = 0	
	where
		(_,idadeRg,_) = pessoa rg
	
encontraMaior :: Int -> Dados -> Int -> Int
encontraMaior maxRg maiorAtual rg
    |rg <= maxRg = max (idadeAtual) (encontraMaior maxRg maiorAtual (rg + 1))
    | otherwise = idadeMaior
    where
        (_, idadeMaior, _) = maiorAtual
        pessoaAtual = pessoa rg
        (_, idadeAtual, _) = pessoaAtual