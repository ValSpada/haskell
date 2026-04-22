----------------------------------------- Clase 4 -----------------------------------------

{-
Funcion show: Devuelve la representacion en string de aquello que le pasamos.

1) Saber si un numero es aesthetic, esto se da cuando la cantidad de dígitos es igual al sentido
   de la vida... 42

2) Saber si un numero es multiplo de otro.

3) Es frase cheta, esta tiene una cantidad de caracteres múltiplo de 5.
-}

-- 1)
-- esAesthetic :: Integral a => a -> Bool
-- esAesthetic numero = div numero (10 ^ 41) == 1
--                   lenght (show numero) == 42
--                   ((== 42) . length . show) numero

-- 2)
esMultiploDe :: Integral a => a -> a -> Bool
esMultiploDe posibleMultiplo numeroBase = ((== 0) . mod posibleMultiplo) numeroBase
--           numerador divisor = (== 0) . mod numerador $ divisor
--                             = 0 == (mod numerador divisor)
-- esMultiploDe num = (== 0) . mod num

-- 3)
esFraseCheta :: String -> Bool
-- esFraseCheta frase = (== 0) . (`mod` 5) . length $ frase
-- esFraseCheta frase = esMultiploDe 5 . length $ frase
-- esFraseCheta frase = ((flip esMultiploDe 5) . lenght) frase

-- esFraseCheta = (`esMultiploDe` 5) . length
-- \x y -> x + y
{-
Debido a que la composicion es con funciones que esperan un valor, se usa aplicación parcial.

esAesthetic = (== 42) . lenght . show / Trabajamos con funciones ahora, interesa usar la funcion
                                      / y no el como funciona dentro. Es siempre con composición.
-}

-- 3) Usando flip e igualando funciones como si fueran valores.
esFraseCheta = flip esMultiploDe 5 . length

---------- Listas ----------
{-
Se hace entre corchetes []
-> Vacia: []
-> Elementos: [1,2,3,4,5,6]
-> Funciones: [odd, even] :: Integral a => [a -> Bool] -- Todas las funciones deben cumplir el mismo T
Elementos :: Num a => [a]
El tipo de la lista esta dada por su contenido

['h','o','l','a'] = "hola" -- Lista de caracteres = String

En la lista no se combinan elementos del mismo tipo

head [1, 2, 3] -> head :: [a] -> a   / Devuelve 1
last [1, 2, 3] / Devuelve 3
tail [1, 2, 3] -> tail :: [a] -> [a] / Devuelve la cola de las listas [2, 3, 4]

tail [1] / Devuelve []
head []  / Devuelve Error, ambas trabajan con al menos un elemento

null []  -> True
null [1] -> False

:t tail [1]
tail [1] :: Num a => [a] / Devuelve una lista con el mismo valor original

saludar alguien = "hola, " ++ alguien
saludar "Fede" -> "hola, Fede"

Toda operacion definida para Strings, se define para listas

reverse [1, 2, 3] -> [3, 2, 1]

:t (++)
(++) ::\x y -> x + y [a] -> [a] -> [a]

[2] > [1]
True
Equiparar listas depende de sus elementos

sum [1,2,3,4,5,6,7,8,9,10] -> 55 / Suma sus elementos / :t Lista de numeros
product -> Devuelve multiplicacion

elem [1] [[1], [2], [3]] -> True / El elemento se compara con lo que contiene la lista
:t elem -> elem :: Eq a => a -> [a] -> Bool

pdep.com.ar -> Guia de lenguajes con toda la data

[1, 2, 3, 4, 5] !! 4 -> 5 / (!!) :: [a] -> Int -> a

:t (:) -> a -> [a] -> [a]
1 : [2,3,4] -> [1,2,3,4] / Devuelve una nueva lista cuya cabeza es el elemento que le pasamos
-}

{-
Funciones de orden superior para listas
-}

numeros = [1,2,3,4,5]

doble = (* 2)

siguiente = (+ 1)

esAesthetic :: Integer  -> Bool
esAesthetic = (== 42) . length . show

{-
filter :: (a -> Bool) -> [a] -> [a] / Dada una funcion, aplica la condicion y devuelve una lista
                                      con los parametros que verifican la condicion (funcion
                                      que devuelve Bool)
                                    / Operador -> Funcion binaria
filter id [True, False, True, True, False] / Devuelve solo los True
filter not / Devuelve los False
-}

{-
map::(a -> b) -> [a] -> [b]
map siguiente [1,2,3]

map (doble . length) ["hola", "como", "estas"] -> [8,8,10]

:t all -> (a -> Bool) -> [a] -> Bool / Si todos la cumplen
:t any -> (a -> Bool) -> [a] -> Bool / Al menos uno la cumple
-}

-- POST RECREO

{-
Calculo Lambda -> Definir funciones con ese caracter: lambda x.x+x / Haskell se para en esto

-}

-- (\x -> x + x) -> Funcion doble
-- \numero -> numero + numero / Expresion lambda y sirve para desarrollar en consola o pasar como
--                              parametro para no ponerme a trabajar en nuevas funciones o en funciones
--                              que no podemos crear de alguna otra forma.

suma :: Int -> Int -> (Int)
suma x y = x + y

suma' :: Int -> (Int -> Int)
suma' x = \y -> x + y -- Se le puede hacer aplicacion parcial
-- (\x y -> x + y) 2 --> (\y -> 2 + y)

suma'' :: (Int -> (Int -> (Int))) -- > Haskell interpreta que solo hay funciones de un parametro, 
                                  -- si tiene mas de uno le das un parametro y devuelve una funcion
suma'' = \x -> \y -> x + y -- ASÍ LO VE HASKELL / :t -> Integer -> Integer -> Integer\x y -> x + y

---------- CURRIFICACION ----------
-- Currificación: Toda funcion recibe un parametro y devuelve otra funcion. (Par. Funcional)
-- Haskell currifica por default en cada función => Tenemos aplicación parcial

{-
(.) :: (b -> c) -> (a -> b) -> a -> c 
Con la currificacion, podemos ver que b es una función como mod:: Int -> (Int -> Bool)
siendo el primer Int el valor "a" y la otra funcion el valor "b"
-}

-- Desafio Almuerzo: :t (.) -> Espera dos funciones (parametros)
-- :t (.)( . )( . ) inferir el tipo y explicarlo por DM porque llegamos a eso