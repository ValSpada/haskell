{-
1. Definir las funciones fst3, snd3, trd3, que dada una tupla de 3 elementos devuelva el 
   elemento correspondiente, p.ej. 
   Main> snd3 (4,5,6) 
   5
   Main> trd3(4,5,6)
   6
-}

fst3 :: (a, b, c) -> a
fst3 (primero,_,_) = primero

snd3 :: (a, b, c) -> b
snd3 (_,segundo,_) = segundo

trd3 :: (a, b, c) -> c
trd3 (_,_,tercero) = tercero

------------------------------------------------------------------------------------------------------

{-
2. Definir la función aplicar, que recibe como argumento una tupla de 2 elementos con funciones 
   y un entero, me devuelve como resultado una tupla con el resultado de aplicar el elemento a 
   cada una de la funciones, ej: 
   Main> aplicar (doble,triple) 8 
   (16,24) 
   Main> aplicar ((3+),(2*)) 8 
   (11,16)
-}

aplicar :: ((Int -> a),(Int -> b)) -> Int -> (a,b)
aplicar (funcion1, funcion2) numero = (funcion1 numero, funcion2 numero)

------------------------------------------------------------------------------------------------------

{-
3. Definir la función cuentaBizarra, que recibe un par y: si el primer elemento es mayor al 
   segundo devuelve la suma, si el segundo le lleva más de 10 al primero devuelve la resta 2do – 1ro,
   y si el segundo es más grande que el 1ro pero no llega a llevarle 10, devuelve el producto. Ej: 
   Main> cuentaBizarra (5,8)
   40
   Main> cuentaBizarra (8,5)
   13
   Main> cuentaBizarra (5,29)
   24
-}

cuentaBizarra :: (Int, Int) -> Int
cuentaBizarra (primerNumero, segundoNumero)
    | primerNumero > segundoNumero      = primerNumero + segundoNumero
    | segundoNumero > primerNumero + 10 = segundoNumero - primerNumero
    | otherwise                         = primerNumero * segundoNumero

------------------------------------------------------------------------------------------------------

{-
4. Representamos las notas que se sacó un alumno en dos parciales mediante un par (nota1,nota2), p.ej. 
   un patito en el 1ro y un 7 en el 2do se representan mediante el par (2,7). 
   A partir de esto: 
    a. Definir la función esNotaBochazo, recibe un número y devuelve True si no llega a 6, False en 
       caso contrario. No vale usar guardas. 
    b. Definir la función aprobo, recibe un par e indica si una persona que se sacó esas notas aprueba. 
       Usar esNotaBochazo. 
    c. Definir la función promociono, que indica si promocionó, para eso tiene las dos notas tienen 
       que sumar al menos 15 y además haberse sacado al menos 7 en cada parcial. 
    d. Escribir una consulta que dado un par indica si aprobó el primer parcial, usando esNotaBochazo 
       y composición. La consulta tiene que tener esta forma (p.ej. para el par de notas (5,8)) 
       Main> (... algo ...) (5,8) 
-}

-- Punto a.
esNotaBochazo :: Int -> Bool
esNotaBochazo = (> 6)

-- Punto b.
aprobo :: (Int,Int) -> Bool
aprobo (nota1,nota2) = esNotaBochazo nota1 && esNotaBochazo nota2

-- Punto c.
promociono :: (Int,Int) -> Bool
promociono (nota1,nota2) = aprobo (nota1,nota2) && nota1 + nota2 >= 15 && (nota1 >= 7 || nota2 >= 7)

-- Punto d.
consulta :: String -> (Int,Int) -> Bool
consulta consultar (nota1,nota2)
    | consultar == "Primer Parcial"  = esNotaBochazo nota1
    | consultar == "Segundo Parcial" = esNotaBochazo nota2

------------------------------------------------------------------------------------------------------

{-
5. Siguiendo con el dominio del ejercicio anterior, tenemos ahora dos parciales con dos recuperatorios,
   lo representamos mediante un par de pares ((parc1,parc2),(recup1,recup2)). 
   Si una persona no rindió un recuperatorio, entonces ponemos un "-1" en el lugar correspondiente. 
   Observamos que con la codificación elegida, siempre la mejor nota es el máximo entre nota del parcial 
   y nota del recuperatorio. 
   Considerar que vale recuperar para promocionar. En este ejercicio vale usar las funciones que se 
   definieron para el anterior. 
    a. Definir la función notasFinales que recibe un par de pares y devuelve el par que corresponde a 
       las notas finales del alumno para el 1er y el 2do parcial. P.ej. 
       Main> notasFinales ((2,7),(6,-1)) 
       (6,7) 
       Main> notasFinales ((2,2),(6,2)) 
       (6,2) 
       Main> notasFinales ((8,7),(-1,-1)) 
       (8,7) 
    b. Escribir la consulta que indica si un alumno cuyas notas son ((2,7),(6,-1)) recursa o no. O sea, 
       la respuesta debe ser True si recursa, y False si no recursa. Usar las funciones definidas en 
       este punto y el anterior, y composición. La consulta debe tener esta forma:
       Main> (... algo ...) ((2,7),(6,-1)) 
    c. Escribir la consulta que indica si un alumno cuyas notas son ((2,7),(6,-1)) recuperó el primer 
       parcial. Usar composición. La consulta debe tener esta forma:
       Main> (... algo ...) ((2,7),(6,-1))
    d. Definir la función recuperoDeGusto que dado el par de pares que representa a un alumno, devuelve 
       True si el alumno, pudiendo promocionar con los parciales (o sea sin recup.), igual rindió al 
       menos un recup. Vale definir funciones auxiliares. Hacer una definición que no use pattern 
       matching, en las eventuales funciones auxiliares tampoco; o sea, manejarse siempre con fst y snd.
-}

-- Punto a.
notasFinales :: ((Int,Int),(Int,Int)) -> (Int,Int)
notasFinales ((nota1,nota2),(recu1,recu2))
    | nota1 < recu1 && nota2 < recu2 = (recu1, recu2)
    | nota1 < recu1                  = (recu1, nota2)
    | nota2 < recu2                  = (nota1, recu2)
    | otherwise                      = (nota1, nota2)

-- Punto b.
recursa :: ((Int,Int),(Int,Int)) -> Bool
recursa = not . aprobo . notasFinales

-- Punto c.
aproboPrimerParcial :: ((Int,Int),(Int,Int)) -> Bool
aproboPrimerParcial = esNotaBochazo . fst . snd

-- Punto d.
recuperoDeGusto :: ((Int,Int),(Int,Int)) -> Bool
recuperoDeGusto notas = (promociono . fst) notas && ((fst . snd) notas /= -1 || (snd . snd) notas /= -1)

------------------------------------------------------------------------------------------------------

{-
6. Definir la función esMayorDeEdad, que dada una tupla de 2 elementos (persona, edad) me devuelva 
   True si es mayor de 21 años y False en caso contrario. Por Ej:.
   Main> esMayorDeEdad (juan,18) 
   False 
   Nota: Definir la función utilizando aplicación parcial y composición.
-}

esMayorDeEdad :: (String,Int) -> Bool
esMayorDeEdad = (>= 21) . snd

------------------------------------------------------------------------------------------------------

{-
7. Definir la función calcular, que recibe una tupla de 2 elementos, si el primer elemento es par lo 
   duplica, sino lo deja como está y con el segundo elemento en caso de ser impar le suma 1 y si no 
   deja esté último como esta. 
   Main> calcular (4,5)
   (8,6) 
   Main> calcular (3,7)
   (3,8) 
   Nota: Resolverlo utilizando aplicación parcial y composición. 
-}

calcular :: (Int,Int) -> (Int,Int)
calcular numeros
    | (even . fst) numeros && (odd . snd) numeros = (((2 *) . fst) numeros, ((1 +) . snd) numeros)
    | (even . fst) numeros                        = (((2 *) . fst) numeros, snd numeros)
    | otherwise                                   = (fst numeros, ((1 +) . snd) numeros)