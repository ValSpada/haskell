----------------------------------------- BOLUDECES -----------------------------------------

------------------ TUPLAS ------------------

-- Tuplas = (Sustancia,INT)
-- EJEMPLO QUIMICA -> [(Sustancia,INT)]

-- FUNCION: fst :: (a,b) -> a
-- FUNCION: snd :: (a,b) -> b
-- PATTERN MATCHING -> fst (x,_) = x
-- PATTERN MATCHING -> snd (_,y) = y

-- EN CASO DE MÁS DE DOS ELEMENTOS
-- fst3 (x,_,_) = x
-- snd3 (_,y,_) = y
-- trd3 (_,_,z) = z

-- EL PROBLEMA: Su tipo de dato es tupla de...

------------------ ALIAS DE TIPO ------------------

-- Haskell permite crear un alias de un tipo:
-- type Componente = (Sustancia, Int)
-- type String = [Char]

----------------------------------------- RECURSIVIDAD -----------------------------------------

factorial :: Integer -> Integer
-- factorial 0      = 1
-- factorial numero = product $ [1..numero]
factorial numero
    | numero == 0 = 1 -- Caso base
    | numero >= 1 = numero * factorial (numero - 1) -- Caso recursivo

fibonacci :: Integer -> Integer
fibonacci numero
    | numero == 0 = 0 -- Va a existir un caso base (o más de uno)
    | numero == 1 = 1
    | numero >  1 = fibonacci (numero - 1) + fibonacci (numero - 2) -- Al menos un caso recursivo

-- DEPENDIENDO LA CANTIDAD DE LLAMADAS RECURSIVAS, SE DARÁ UNA CIERTA CANTIDAD DE CASOS BASE
-- ¿Numero de Ackerman?

-- LAS LISTAS SON RECURSIVAS TAMBIÉN
-- [1,2,3] -> 1:[2,3] -> 1:2:[3] -> 1:2:3:[]
-- LA LISTA VACIA ES EL CASO BASE

-- DEFINICION DE LISTA -> data [a] = a : [a]

-- Funciones recursivas -> sum, product, and, or, concat, length, take, drop, map, filter, any, all

-- FUNCION QUE RECIBE UNA LISTA Y DEVUELVE LA SUMATORIA DE TODAS LAS COMPONENTES
sum' :: Num a => [a] -> a
sum' [] = 0 -- EL CASO BASE ESTA SIENDO EL NEUTRO
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length' []     = 0 -- EL CASO BASE ESTA SIENDO EL NEUTRO
length' (_:xs) = 1 + length' xs

and' :: [Bool] -> Bool
and' [] = True -- EL CASO BASE ESTA SIENDO EL NEUTRO
and' (x:xs) = x && and' xs

or' :: [Bool] -> Bool
or' [] = False -- EL CASO BASE ESTA SIENDO EL NEUTRO
or' (x:xs) = x || and' xs

concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs

take' :: Int -> [a] -> [a]
take' 0 _             = []
take' cantidad (x:xs) = [x] ++ take' (cantidad - 1) xs

drop' :: Int -> [a] -> [a]
drop' _ []             = []
-- drop' cantidad (x:xs)  = [x] ++ drop' (cantidad - 1) xs

-- GENERALIZAMOS LA CREACIÓN
-- plegar op neutro []     = neutro
-- plegar op neutro (x:xs) = x ´op´ plegar op neutro xs
plegar :: (b -> a -> a) -> a -> [b] -> a
plegar operacion neutro [] = neutro
plegar operacion neutro (x:xs) = x `operacion` plegar operacion neutro xs

sumatoria :: Num a => [a] -> a
sumatoria = plegar (+) 0

productoria :: Num a => [a] -> a
productoria lista = plegar (*) 1 lista

conjuncion :: [a] -> a
conjuncion lista = plegar (&&) True lista

disyuncion :: [a] -> a
disyuncion lista = plegar (||) False lista

concatenacion :: [a] -> a
concatenacion lista = plegar (++) [] lista

--longitud :: [a] -> a
--longitud lista = plegar (\_ algo -> 1 + algo) 0 lista

-- foldl -> Plegar a izquierda
-- foldr -> Plegar a derecha
-- Foldable -> LISTAS PARA APLANAR
-- fold -> reduce una lista a un valor

-- SIEMPRE QUE SE PUEDA USAR FOLD, MEJOR. EVITAR RECURSIVIDAD

----------------------------------------- GITHUB -----------------------------------------

-- GitHub -> Repositorio de Git -> Sistema de control de versiones de codigo
-- Versionar -> Historicamente se hacia con disquettes/mail, para mejorar el sistema se
-- creó el versionado