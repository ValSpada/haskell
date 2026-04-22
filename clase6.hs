------------------------------------- Listas Pot. Infinitas -------------------------------------

{-
Armar listas indicando incrementos -> [0,9..100] MULTIPLOS DE 9 = enumFromThenTo

[1..10] = enumFromTo

[1..] -> No hay cota superior => Listas infinitas o potencialmente infinitas

repeat -> Repite el valor dado potencialmente infinito
:t -> a -> [a]

cycle -> Repite cada valor de la lista dada
cycle listaInfinita = listaInfinita -> En resultado

iterate -> Aplica la funcion al elemento y la itera 
:t ->(a -> a) -> a -> [a]
[x,f(x),f(f(x)),f(f(f(x)))]

replicate -> Segun la cantidad dada, genera una lista finita con el elemento
:t -> Int -> a -> [a]

-}

-- FUN FACT -> (,) si le pasamos dos parametros, arma la tupla

repetir :: a -> [a]
repetir x = x : repetir x -- > No hay caso base, DIVERGE => NO FRENA

ciclar :: [a] -> [a]
ciclar xs = xs ++ ciclar xs 

iterar :: (a -> a) -> a -> [a]
iterar f x = x : iterar f (f x)

replicar :: Int -> a -> [a]
replicar 0 _ = []
replicar n x = x : replicar (n - 1) x

-- COMO OPERARLAS Y SU FUNCION

-- head [1..] -> 1
-- Haskell es chad y sabe hacer estas cosas

-- Dependiendo la operacion, se pueden operar estas listas o no

-- EVALUACION PEREZOSA = LAZY EVALUATION -> No evalua cosas que no necesita hasta que no sea necesario
-- EVALUACION ANSIOSA  = EAGER EVALUATION -> Evalua todos los parametros y luego ejecuta

--doble x = x + x

------ EAGER EVALUATION  / Call by value => Resuelve primero el parametro ------
-- doble (doble 2)
-- doble (2+2)
-- doble (4)
-- doble (4+4)
-- doble (8)

------ LAZY EVALUATION  / Call by name => Resuelve primero la funcion ------
-- (doble 2) + (doble 2)
-- (2 + 2) + (doble 2)
-- (4) + (2 + 2)
-- 4 + 4
-- 8

-- Sharing + Call by name = LAZY EVALUATION --
-- (doble 2) + (doble 2)
-- (2 + 2) + (2 + 2)
-- 4 + 4
-- 8

-- Esto es gracias a la inmutabilidad que propone HASKELL, el resultado siempre es igual

n1 = 7
n2 = 9
suma = n1 + n2 -- ESTO SIEMPRE DARA 16, SE REDUCE A ESO

-- Paradigma Funcional => LAZY EVALUATION

-- fst (37,undefined)

-- HASKELITE -> Evalua funciones de haskell paso por paso

-- foldl y foldr -> Verificar en que lado se van aplicando para operaciones no conmutativas

------------------------------------- BOLUDECES -------------------------------------