{-
1. Definir la función existsAny/2, que dadas una función booleana y una tupla de tres elementos 
   devuelve True si existe algún elemento de la tupla que haga verdadera la función. 
   Main> existsAny even (1,3,5) 
   False 

   Main> existsAny even (1,4,7) 
   True 
   porque even 4 da True 

   Main> existsAny (0>) (1,-3,7) 
   True 
   porque even -3 es negativo 
-}

existsAny :: (a -> Bool) -> (a, a, a) -> Bool
existsAny funcion tripla = (funcion . fst3) tripla || (funcion . snd3) tripla || (funcion . trd3) tripla

-- Auxiliar
fst3 :: (a, b, c) -> a
fst3 (primero,_,_) = primero

snd3 :: (a, b, c) -> b
snd3 (_,segundo,_) = segundo

trd3 :: (a, b, c) -> c
trd3 (_,_,tercero) = tercero

----------------------------------------------------------------------------------------------

{-
2. Definir la función mejor/3, que recibe dos funciones y un número, y devuelve el resultado
   de la función que dé un valor más alto. Por ejemplo:
   *Main> mejor cuadrado triple 1
   3
   (Pues triple 1 = 3 > 1 = cuadrado de 1)

   *Main> mejor cuadrado triple 5
   25
   (Pues cuadrado 5 = 25 > 15 = triple 5)

   Nota: No olvidar la función max.
-}

mejor :: (Int -> Int) -> (Int -> Int) -> Int -> Int
mejor funcionUno funcionDos numero = max (funcionUno numero) (funcionDos numero)

-- Auxiliar
cuadrado :: Int -> Int
cuadrado = (^ 2)

triple :: Int -> Int
triple = (3 *)

----------------------------------------------------------------------------------------------

{-
3. Definir la función aplicarPar/2, que recibe una función y un par, y devuelve el par que 
   resulta de aplicar la función a los elementos del par. P.ej. 
   Main> aplicarPar doble (3,12) 
   (6,24) 

   Main> aplicarPar even (3,12) 
   (False, True) 

   Main> aplicarPar (even . doble) (3,12) 
   (True, True) 
-}

aplicarPar :: (a -> b) -> (a, a) -> (b, b)
aplicarPar funcion tupla = ((funcion . fst) tupla, (funcion . snd) tupla) 

-- Auxiliar
doble :: Int -> Int
doble = (2 *)

----------------------------------------------------------------------------------------------

{-
4. Definir la función parDeFns/3, que recibe dos funciones y un valor, y devuelve un par 
   ordenado que es el resultado de aplicar las dos funciones al valor. P.ej. 
   Main> parDeFns even doble 12 
   (True, 24) 
-}

parDeFns :: (a -> b) -> (a -> c) -> a -> (b, c)
parDeFns funcion1 funcion2 valor = (funcion1 valor, funcion2 valor)