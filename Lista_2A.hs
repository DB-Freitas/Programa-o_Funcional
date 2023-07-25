import Data.Char

-- Ex 1 ---------
converte::Char->(Char,Char,Int)
converte b
	|isLower b = (b,(toUpper b),(ord b))
	|otherwise = (b,(toLower b),(ord b))
	
-- Ex 2 ---------
---nome da pessoa de menor idade até um determinado registro
type Dados = (String,Int,Char)
pessoa rg 
	|rg == 1 = ("Joao Silva", 12, 'm')
	|rg == 2 = ("Jonas Souza", 59, 'm')
	|rg == 3 = ("Jocileide Strauss" , 51, 'f')
	|rg == 4 = ("Jocileide Strauss" , 52, 'f')
	|otherwise = ("Não ha ninguem mais", -1, 'x')

-- a)
menorIdade::Int->String
menorIdade rg = (menorIdade1 rg (pessoa rg))


menorIdade1::Int->Dados->String
menorIdade1 0 _ = ""
menorIdade1 rg (n,i,s)
	|i < 100 = n ++ "." ++ menorIdade1 (rg-1) (pessoa (rg-1))
	|otherwise = menorIdade1 (rg-1) (pessoa (rg-1))
	
-- b)
somaIdade::Int->Dados->Int
somaIdade 0 _ = 0
somaIdade rg (n,i,s)
	|i/= 9999 = i + (somaIdade (rg-1) (pessoa (rg-1)))
	|otherwise = (somaIdade (rg-1) (pessoa (rg-1)))

idadeMedia::Int->Float
idadeMedia rg = fromIntegral(somaIdade rg (pessoa rg)) / (fromIntegral rg)


-- c)
numMasc::Int->Int
numMasc rg = (numMasc1 rg (pessoa rg))

numMasc1::Int->Dados->Int
numMasc1 0 _ = 0
numMasc1 rg (n,i,s)
	|s == 'm' = 1 + (numMasc1 (rg-1) (pessoa (rg-1)))
	|otherwise = (numMasc1 (rg-1) (pessoa (rg-1)))

-- d)****
idade::Dados->Int
idade (n,i,s) = i

rgMaiorIdade::Int->Int
rgMaiorIdade rg = (rgMaiorIdade1 rg (pessoa rg))

rgMaiorIdade1::Int->Dados->Int
rgMaiorIdade1 0 _ = 0
rgMaiorIdade1 rg (n,i,s)
	|idade (pessoa rg) > idade (pessoa (rg-1)) = rg
	|otherwise = rgMaiorIdade (rg-1)
	
	
-- e) lista de rg de pessoas de idade > 18
rgMaiorIdadeList::Int->[Int]
rgMaiorIdadeList rg = (rgMaiorIdadeL rg (pessoa rg))

rgMaiorIdadeL::Int->Dados->[Int]
rgMaiorIdadeL 0 _ = []
rgMaiorIdadeL rg (n,i,s)
	|i>=18 && i/=9999 = [rg] ++ rgMaiorIdadeL (rg-1) (pessoa (rg-1))
	|otherwise = rgMaiorIdadeL (rg-1) (pessoa (rg-1))
	
-- Ex 3 ---------
--mesmo do Ex1

-- Ex 4 ---------
type T_Int4 = (Int,Int,Int,Int)

insere::Int->[Int]->[Int]
insere e [] = [e]
insere e (a:x)
	| e<=a = e:(a:x)
	| otherwise = a : insere e x

ordenacao::[Int]->[Int]
ordenacao [] = []
ordenacao (a:x) = insere a (ordenacao x)

insTupOrd::[Int]->T_Int4
insTupOrd (a:b:c:[d]) = (a,b,c,d)

ordena::Int->Int->Int->Int->T_Int4
ordena a b c d = insTupOrd (ordenacao (a:b:c:[d]))

-- Ex 5 ---------
type Data = (Int,Int,Int)

converteDias::Data->Int
converteDias (d,m,a) 
	|(mod a 4) == 0 = d + (m*30) + (a*366)
	|otherwise = d + (m*30) + (a*365)

diasEntreDatas::Data->Data->Int
diasEntreDatas d1 d2 = (converteDias d2) - (converteDias d1)

-- Ex 6 ---------
delta::(Int,Int,Int)->Int
delta (a,b,c) = b^2 - 4*a*c

raiz1::(Int,Int,Int)->Int->Float
raiz1 (a,b,c) d = ((fromIntegral (-b)) + sqrt (fromIntegral (d))) / ( 2 * fromIntegral a)

raiz2::(Int,Int,Int)->Int->Float
raiz2 (a,b,c) d = ((fromIntegral (-b)) - sqrt (fromIntegral (d))) / ( 2 * fromIntegral a)

equacao::(Int,Int,Int)->(Float,Float)
equacao e
	|delta e >=0 = (raiz1 e (delta e),raiz2 e (delta e))
	|otherwise = (9999,9999)
	
-- Ex 7 --------
somaT::(Int,Int,Int)->Int
somaT (a,b,c) = a+b+c

triangulo::(Int,Int,Int)->(String,Int)
triangulo (a,b,c)
	|a==b && a==c = ("Equilatero", somaT (a,b,c))
	|a/=b && b/=c && a/=c = ("Escaleno", somaT (a,b,c))
	|otherwise = ("Isoceles", somaT (a,b,c))
	
-- Ex 8 --------
type DadosBase = (Int,String,String,Char)
base::Int->DadosBase
base x
	|x == 0 = (1793, "Pedro Paulo" ,"MESTRE", 'M')
	|x == 1 = (1797, "Joana Silva Alencar","MESTRE", 'M')
	|x == 2 = (1534, "Joao De Medeiros" ,"DOUTOR", 'F')
	|x == 3 = (1267, "Claudio Cesar de Sa","DOUTOR", 'M')
	|x == 4 = (1737, "Paula de Medeiros" ,"MESTRE", 'F')
	|x == 5 = (1888, "Rita de Matos" ,"MESTRE", 'F')
	|x == 9 = (1698, "Tereza Cristina Andrade" ,"MESTRE" , 'F')
	|otherwise = (0,"","",'O')
	
-- a)
numDoutoresBase::Int
numDoutoresBase = numDoutores (base 10) 10

numDoutores::DadosBase->Int->Int
numDoutores (a,n,t,s) i
	|(t == "DOUTOR") && (i>=0) = 1 + numDoutores (base (i-1)) (i-1)
	|i < 0 = 0
	|otherwise = numDoutores (base (i-1)) (i-1)

-- b)
numMulheresBase::Int
numMulheresBase = numMulheres (base 10) 10

numMulheres::DadosBase->Int->Int
numMulheres (a,n,t,s) i
	|(s=='F') && (i>=0) = 1 + numMulheres (base (i-1)) (i-1)
	|i < 0 = 0
	|otherwise = numMulheres (base (i-1)) (i-1)

-- c)
numMestresMBase::Int
numMestresMBase = numMestresM (base 10) 10

numMestresM::DadosBase->Int->Int
numMestresM (a,n,t,s) i
	|(t == "MESTRE") && (s == 'M') && (i>=0) = 1 + numMestresM (base (i-1)) (i-1)
	|i < 0 = 0
	|otherwise = numMestresM (base (i-1)) (i-1)

-- d)****
profMaisAntigo::String
profMaisAntigo = maisAntigo (base 10) 10

maisAntigo::DadosBase->Int->String
maisAntigo (a,n,t,s) i
	|i<0 = "Erro"
	|aux>a && i>=0 = n
	|otherwise = maisAntigo (base (i-1)) (i-1)
	where
		aux = a






