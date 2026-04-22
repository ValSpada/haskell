{-
1. Definir una función siguiente, que al invocarla con un número cualquiera me
   devuelve el resultado de sumar a ese número el 1.
   Main> siguiente 3
   4
-}

siguiente :: Num a => a -> a
-- siguiente numero = (+ 1) numero
siguiente = (+ 1)

-------------------------------------------------------------------------------------

{-
2. Definir la función mitad, que al invocarla con un número cualquiera me devuelve la
   mitad de dicho numero, ej:
   Main> mita 5
   2.5
-}

mitad :: Fractional a => a -> a
-- mitad numero = (/ 2) numero
mitad = (/ 2)

-------------------------------------------------------------------------------------

{-
3. Definir una función inversa que invocando a la función con un número caulquiera
   me devuelva la mitad de dicho número, ej:
   Main> inversa 4
   0.25
   Main> inversa 0.5
   2.0
-}

inversa :: Fractional a => a -> a
-- inversa numero = (1 /) numero
inversa = (1 /)

-------------------------------------------------------------------------------------

{-
4. Definir la función triple, que invocando a la función con un número cualquiera
   me devuelva el triple del mismo.
   Main> triple 5
   15
-}

triple :: Num a => a -> a
-- triple numero = (* 3) numero
triple = (* 3)

-------------------------------------------------------------------------------------

{-
5. Definir la función esNumeroPositivo, que invocando a la función con un número
   cualquiera me devuelva True si el número es positivo y False en caso contrario.
   Main> esNumeroPositivo (-5)
   False
   Main> esNumeroPositivo 0.99
   True
-}

esNumeroPositivo :: (Num a, Ord a) => a -> Bool
-- esNumeroPositivo numero = (> 0) numero
esNumeroPositivo = (> 0)

-------------------------------------------------------------------------------------

{-
6. Resolver la función del ejercicio 2 de la guía anterior esMultiploDe/2, utilizando
   aplicación parcial y composición.
-}

esMultiploDe :: Int -> Int -> Bool
esMultiploDe posibleMultiplo numeroBase = (== 0) . mod posibleMultiplo $ numeroBase 

-------------------------------------------------------------------------------------

{-
7. Resolver la función del ejericico 5 de la guía anterior esBisiesto/1, utilizando
   aplicación parcial y composición.
-}

esBisiesto :: Int -> Bool
--esBisiesto año = ((&& ((|| (esMultiploDe año) 400) . (/= 0) . mod año) 100) . esMultiploDe año) 4
esBisiesto año = ((&& ((|| (esMultiploDe año) 400) . not . esMultiploDe año) 100) . esMultiploDe año) 4

-------------------------------------------------------------------------------------

{-
8. Resolver la función inversaRaizCuadrada/1, que da un número n devolver la
   inversa de su raíz cuadrada.
   Main > inversaRaizCuadrada 4
   0.5
   Nota: Resolverlo utilizando la función inversa. Ej: 2.3, sqrt y composición.
-}

inversaRaizCuadrada :: Floating a => a -> a
-- inversaRaizCuadrada numero = ((1 /) . sqrt) numero
inversaRaizCuadrada numero = inversa . sqrt $ numero

-------------------------------------------------------------------------------------

{-
9. Definir una función incrementMCuadradoN, que invocandola con 2 números m y n,
   incrementa un valor m al cuadrado de n por ej:
   Main> incrementMCuadradoN 3 2
   11
   Incrementa 2 al cuadrado de 3, da como resultado 11. Nota: Resolverlo utilizando
   aplicación parcial y composición.
-}

incrementMCuadradoN :: Num a => a -> a -> a
incrementMCuadradoN m n = ((+ n) . (^ 2)) m

-------------------------------------------------------------------------------------

{-
10. Definir una función esResultadoPar/2, que invocándola con número n y otro m,
    devuelve True si el resultado de elevar n a m es par.
    Main> esResultadoPar 2 5
    True
    Main> esResultadoPar 3 2
    False
    Nota Obvia: Resolverlo utilizando aplicación parcial y composición.
-}

esResultadoPar :: Integral a => a -> a -> Bool
-- esResultadoPar base potencia = (== 0) . (flip mod 2) . (^ potencia) $ base
esResultadoPar base potencia = even . (^ potencia) $ base