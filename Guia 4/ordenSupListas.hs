import Data.List
import Text.Show.Functions

----------------------------------------------------------------------------------------------

{-
1. Definir la función esMultiploDeAlguno/3, que recibe un numero y una lista y devuelve True si el
   numero es multiplo de alguno de los numeros de la lista. Por ejemplo:
   *Main> esMultiploDeAlguno 15 [2,3,4]
   True
   (Porque 15 es multiplo de 3)

   *Main> esMultiploDeAlguno 34 [2,3,4]
   False
   (Porque 34 no es multiplo de ninguno de los 3)

   Nota: Utilizar la funcion any/2.
-}

esMultiploDeAlguno :: Int -> [Int] -> Bool
esMultiploDeAlguno numero lista = any (esMultiploDe numero) lista

--Auxiliar
esMultiploDe :: Int -> Int -> Bool
esMultiploDe multiplo base = (== 0) . mod multiplo $ base 

------------------------------------------------------------------------------------------------

{-
2. Armar una funcion promedios/1, que dada una lista de listas me devuelve la lista de los
   promedios de cada lista-elemento. Por ejemplo:
   Main> promedios [[8,6],[7,9,4],[6,2,4],[9,6]]
   [7,6.67,4,7.5]
   Nota: Implementar una solución utilizando map/2.
-}

promedios :: [[Float]] -> [Float]
promedios = map calcularPromedio

calcularPromedio :: [Float] -> Float
calcularPromedio lista = sum lista / Data.List.genericLength lista

------------------------------------------------------------------------------------------------

{-
3. Armar una funcion promediosSinAplazos/1, que dada una lista de listas me devuelve la lista
   de los promedios de cada lista-elemento, excluyendo los que sean menores a 4 que 
   no se cuentan. Ejemplo: 
   Main> promediosSinAplazos [[8,6],[6,2,6]]
   [7,6]
   Nota: Implementar una solucion utilizando map/2.
-}

promediosSinAplazos :: [[Float]] -> [Float]
promediosSinAplazos = map verificarPromedios

-- Auxiliar
verificarPromedios :: [Float] -> Float
verificarPromedios lista
   | promedio >= 4 = promedio
   | otherwise     = 0
   where promedio = calcularPromedio lista

------------------------------------------------------------------------------------------------

{-
4. Definir la funcion mejoresNotas, que dada la informacion de un curso devuelve la lista con la
   mejor nota de cada alumno. Por ejemplo:
   Main> mejoresNotas [[8,6,2,4],[7,9,4,5],[6,2,4,2],[9,6,7,10]]
   [8,9,6,10]
   Ayuda: Utilizar la funcion predefinida maximum/1
-}

mejoresNotas :: [[Int]] -> [Int]
mejoresNotas = map maximum

------------------------------------------------------------------------------------------------

{-
5. Definir la funcion aprobo/1, que dada la lista de las notas de un alumno devuelve TRue si el
   alumno aprobó. Se dice que un alumno aprobó si todas sus notas son 6 o más. Por ejemplo:
   Main> aprobo [8,6,2,4]
   False
   Main> aprobo [7,9,6,8]
   True
   Ayuda: Utilizar la funcion predefinida minimun/1
-}

aprobo :: [Int] -> Bool
aprobo = (>= 6) . minimum

------------------------------------------------------------------------------------------------

{-
6. Definir la funcion aprobaron/1, que dada la información de un curso devuelve la información
   de los alumnos que aprobaron. Por ejemplo:
   Main> aprobaron [[8,6,2,4],[7,9,6,7],[6,2,4,2],[9,6,7,10]]
   [[7,9,6,7],[9,6,7,10]]
   Ayuda: Usar la función aprobo/1
-}

aprobaron :: [[Int]] -> [[Int]]
aprobaron = filter aprobo

------------------------------------------------------------------------------------------------

{-
7. Definir la funcion divisores/1, que recibe un numero y devuelve la lista de divisores. 
   Por ejemplo:
   Main> divisores 60
   [1,2,3,4,5,6,10,12,15,20,30,60]
   Ayuda: Para calcular divisores n alcanza con revisar los números entre 1 y n
-}

divisores :: Int -> [Int]
divisores = numerosHasta 1

-- Auxiliar
esDivisor :: Int -> Int -> Bool
esDivisor numero divisor = (mod numero divisor) == 0 && divisor < numero

numerosHasta :: Int -> Int -> [Int]
numerosHasta desde hasta
   | esDivisor hasta desde = [] ++ [desde] ++ numerosHasta (desde + 1) hasta
   | desde == hasta        = [] ++ [hasta]
   | otherwise             = [] ++ numerosHasta (desde + 1) hasta

------------------------------------------------------------------------------------------------

{-
8. Definir la funcion exists/2, que dadas una funcion booleana y una lista, devuelve True si la
   función da True para algun elemento de la lista. Por ejemplo:
   Main> exists even [1,3,5]
   False
   Main> exists even [1,4,7]
   True
   Porque even 4 da True
-}

exists :: (a -> Bool) -> [a] -> Bool
exists funcion = any funcion 

------------------------------------------------------------------------------------------------

{-
9. Definir la funcion hayAlgunNegativo/2, que dada una lista de números y un (...algo...) 
   devuelve True si hay algun numero negativo. Por ejemplo:
   Main> hayAlgunNegativo [2,-3,9] (...algo...)
   True
-}

hayAlgunNegativo :: [Int] -> a -> Bool
hayAlgunNegativo lista _ = any (< 0) lista

------------------------------------------------------------------------------------------------

{-
10. Definir la función aplicarFunciones/2, que dadas una lista de funciones y un valor cualquiera,
    devuelve la lista del resultado de aplicar las funciones al valor. Por ejemplo:
    Main> aplicarFunciones [(* 4),(+ 3),abs] (-8)
    [-32,-5,8]
    Si pongo:
    Main> aplicarFunciones [(* 4),even,abs] 8
    da error. ¿Por qué?

    Da error porqué al aplicar las funciones, estamos aplicando 2 funciones que devuelven numeros
    que son (* 4) y abs, pero ademas aplicamos una funcion que devuelve un Bool como even. Por ende,
    estarias devolviendo una lista que no tiene todos sus componentes iguales.
-}

aplicarFunciones :: [(a -> b)] -> a -> [b]
aplicarFunciones funciones valor
   | length funciones /= 0 = [] ++ [obtenerFuncion (head funciones) valor] ++ aplicarFunciones (tail funciones) valor
   | otherwise = []

-- Auxiliar
obtenerFuncion :: (a -> b) -> a -> b
obtenerFuncion funcion valor = funcion valor

------------------------------------------------------------------------------------------------

{-
11. Definir la función sumaF/2, que dadas una lista de funciones y un numero, devuelve la suma
    del resultado de aplicar las funciones al numero. Por ejemplo:
    Main> sumaF [(* 4), (+ 3), abs] (-8)
    -29
-}

sumaF :: Num b => [(a -> b)] -> a -> b
sumaF funciones valor = sum . aplicarFunciones funciones $ valor

------------------------------------------------------------------------------------------------

{-
12. Un programador Haskell está haciendo las cuentas para un juego de fútbol virtual 
    (como el Hattrick o el ManagerZone). En un momento le llega la información sobre la habilidad 
    de cada jugador de un equipo, que es un número entre 0 y 12, y la orden de subir la forma de 
    todos los jugadores en un número entero. 
    Por ejemplo: Subirle 2 la forma a cada jugador. Ahora, ningún jugador puede tener más de 12 
    de habilidad; si un jugador tiene 11 y la orden es subir 2, pasa a 12, no a 13; si estaba en 
    12 se queda en 12. Escribir una función subirHabilidad/2 que reciba un número 
    (que se supone positivo sin validar) y una lista de números, y le suba la habilidad a cada 
    jugador cuidando que ninguno se pase de 12. Por ejemplo:
    Main> subirHabilidad 2 [3,6,9,10,11,12]
    [5,8,11,12,12,12] 
-}

-- Hacer funciones que verifiquen cuando un jugar supera los 12 de forma y jugar con los casos
-- para las guardas si es que un jugador tiene 12 pasa una cosa, sino pasa otra, y así
-- armar una lista de recursividad con estos valores

subirHabilidad :: Int -> [Int] -> [Int]
subirHabilidad subir jugadores = subirJugadores subir jugadores 0

subirJugadores :: Int -> [Int] -> Int -> [Int]
subirJugadores subir jugadores posicion
   | menorDoce(jugadores !! posicion) = [] ++ [subirForma subir (jugadores !! posicion)] ++ subirJugadores subir jugadores siguiente
   | (jugadores !! posicion) == 12    = [] ++ [12]
   | otherwise                        = [] ++ [12] ++ subirJugadores subir jugadores siguiente
   where siguiente = posicion + 1

menorDoce :: Int -> Bool
menorDoce = (< 12)

subirForma :: Int -> Int -> Int
subirForma subir jugador
   | menorDoce (total) = total
   | otherwise         = 12
   where total = subir + jugador

------------------------------------------------------------------------------------------------

{-
13. Ahora el requerimiento es más genérico: hay que cambiar la habilidad de cada jugador según 
    una función que recibe la vieja habilidad y devuelve la nueva. Armar: una función flimitada 
    que recibe una función f y un número n, y devuelve f n garantizando que quede entre 0 y 12 
    (si f n < 0 debe devolver 0, si f n > 12 debe devolver 12). Por ejemplo
    Main> flimitada (*2) 9 
    12 
    Pues 9*2 = 18 > 12 

    Main> flimitada (+(-4)) 3 
    0 
    pues 3-4 = -1 < 0 
 
    Main> flimitada (*2) 5 
    10 
    pues 5*2 = 10 que está en rango 
    Hacerlo en una sola línea y sin guardas. Ayuda: usar min y max.

   a. Definir una función cambiarHabilidad/2, que reciba una función f y una lista de habilidades, 
      y devuelva el resultado de aplicar f con las garantías de rango que da flimitada. P.ej. 
      Main> cambiarHabilidad (*2) [2,4,6,8,10] 
      [4,8,12,12,12] 

   b. Usar cambiarHabilidad/2 para llevar a 4 a los que tenían menos de 4, dejando como estaban 
      al resto. Por ejemplo:
      Main> cambiarHabilidad ... [2,4,5,3,8] 
      [4,4,5,4,8] 
      Lo que hay que escribir es completar donde están los puntitos.
-}

flimitada :: (Int -> Int) -> Int -> Int
flimitada funcion = (acotar 0 12) . funcion

-- Auxiliar
acotar :: Int -> Int -> Int -> Int
acotar cotaInf cotaSup = (min cotaSup) . (max cotaInf)

-- a.
-- cambiarHabilidad :: (Int -> Int) -> [Int] -> [Int]
-- cambiarHabilidad funcion = map (flimitada funcion)

-- b.
cambiarHabilidad :: Int -> [Int] -> [Int]
cambiarHabilidad numero = map (acotar numero 12)

------------------------------------------------------------------------------------------------

{-
14. Investigar lo que hace la función takeWhile/2, que está incluida en el prelude. 
    Preguntar primero el tipo, y después hacer pruebas. Ayudarse con el nombre. 

    Respuesta: 
    La función takeWhile recibe una función que retorna un booleano y una lista, irá aplicando
    la funcón en la lista y devolvera otra lista con los elementos que la cumplen hasta que la
    condicion deje de cumplirse.
    Main> :t takeWhile
    takeWhile :: (a -> Bool) -> [a] -> [a]

    Main> takeWhile (< 0) [-1,-10,10,2]
    [-1,-10]
-}

------------------------------------------------------------------------------------------------

{-
15. Usar takeWhile/2 para definir las siguientes funciones: primerosPares/1, que recibe una 
    lista de números y devuelve la sublista hasta el primer no par exclusive. Por ejemplo:
    Main> primerosPares [4,12,3,8,2,9,6] 
    devuelve [4,12], corta en 3 porque no es par  

    primerosDivisores/2, que recibe una lista de números y un número n, y devuelve la sublista 
    hasta el primer número que no es divisor de n exclusive. Por ejemplo:
    Main> primerosDivisores 60 [4,12,3,8,2,9,6]
    devuelve [4,12,3], corta en 8 porque no divide a 60

    primerosNoDivisores/2, que recibe una lista de números y un número n, y devuelve la sublista 
    hasta el primer número que sí es divisor de n exclusive. P.ej. 
    Main> primerosNoDivisores 60 [8,9,4,12,3,8,2,9,6] 
    devuelve [8,9], corta en 4 porque divide a 60
-}

primerosPares :: [Int] -> [Int]
primerosPares = takeWhile even

primerosDivisores :: Int -> [Int] -> [Int]
primerosDivisores base numeros = takeWhile (esDivisor base) numeros

primerosNoDivisores :: Int -> [Int] -> [Int]
primerosNoDivisores base numeros = takeWhile (not . esDivisor base) numeros

------------------------------------------------------------------------------------------------

{-
16. Se representa la información sobre ingresos y egresos de una persona en cada mes de un año 
    mediante dos listas, de 12 elementos cada una. P.ej., si entre enero y junio gané 100, 
    y entre julio y diciembre gané 120, mi lista de ingresos es:
    [100,100,100,100,100,100,120,120,120,120,120,120] 
    Si empecé en 100 y fui aumentando de a 20 por mes, llegando a 220, queda:
    [100,120..220] 
    Y si es al revés, empecé en 220 y fui bajando de a 20 por mes hasta llegar a 100, queda:
    [220,200..100] 
    (jugar un poco con esta notación) 
    Definir la función: huboMesMejorDe/3, que dadas las listas de ingresos y egresos y un número, 
    devuelve True si el resultado de algún mes es mayor que el número. Por ejemplo
    Main> huboMesMejorDe [1..12] [12,11..1] 10 
    True 
    Porque en diciembre el resultado fue 12-1=11 > 10.
-}

huboMesMejorDe :: [Int] -> [Int] -> Int -> Bool
huboMesMejorDe ingresos egresos numero = any (>= numero) . (zipWith (-) egresos) $ ingresos

------------------------------------------------------------------------------------------------

{-
17. En una población, se estudió que el crecimiento anual de la altura de las personas sigue 
    esta fórmula de acuerdo a la edad:
    1 año: 22 cm 
    2 años: 20 cm 
    3 años: 18 cm 
    ... así bajando de a 2 cm por año hasta 
    9 años: 6 cm 
    10 a 15 años: 4 cm 
    16 y 17 años: 2 cm 
    18 y 19 años: 1 cm 
    20 años o más: 0 cm 
    A partir de esta información: 
      a. Definir la función crecimientoAnual/1,que recibe como parámetro la edad de la persona, 
         y devuelve cuánto tiene que crecer en un año. Hacerlo con guardas. La fórmula para 1 a 10 
         años es 24 - (edad * 2).
      b. Definir la función crecimientoEntreEdades/2, que recibe como parámetros dos edades y 
         devuelve cuánto tiene que crecer una persona entre esas dos edades. P.ej. 
         Main> crecimientoEntreEdades 8 12 
         22 
         es la suma de 8 + 6 + 4 + 4, crecimientos de los años 8, 9, 10 y 11 respectivamente. 
         Nota: Definir la función crecimientoEntreEdades en una sola línea, usando map y suma.
      c. Armar una función alturasEnUnAnio/2, que dada una edad y una lista de alturas de personas, 
         devuelva la altura de esas personas un año después. P.ej. 
         Main> alturasEnUnAnio 7 [120,108,89] 
         [130,118,99] 
         Qué es lo que van a medir las tres personas un año después, dado que el coeficiente de 
         crecimiento anual para 7 años da 10 cm. 
         Nota: definir la función alturasEnUnAnio en una sola línea, usando map y la función 
         (+ expresión).
      d. Definir la función alturaEnEdades/3, que recibe la altura y la edad de una persona y 
         una lista de edades, y devuelve la lista de la altura que va a tener esa persona en 
         cada una de las edades. P.ej. 
         Main> alturaEnEdades 120 8 [12,15,18] 
         [142,154,162] 
         que son las alturas que una persona que mide 120 cm a los 8 años va a tener a los 12, 
         15 y 18 respectivamente.
-}

-- a.
crecimientoAnual :: Int -> Int
crecimientoAnual edad
   | verificar 10 = 24 - (edad * 2)
   | verificar 15 = 4
   | verificar 17 = 2
   | verificar 19 = 1
   | otherwise    = 0
   where verificar = (edad <=)

-- b.
crecimientoEntreEdades :: Int -> Int -> Int
crecimientoEntreEdades menor mayor = sum . (map crecimientoAnual) $ [menor..(mayor - 1)]

-- c.
alturasEnUnAnio :: Int -> [Int] -> [Int]
alturasEnUnAnio edad = map (+ (crecimientoAnual edad))

-- d.
alturaEnEdades :: Int -> Int -> [Int] -> [Int]
alturaEnEdades altura edad = map (+ altura) . map (crecimientoEntreEdades edad)
--alturaEnEdades altura edad []     = []
--alturaEnEdades altura edad edades = [] ++ [altura + (crecimientoEntreEdades 8 (edades !! 0))] ++ alturaEnEdades altura edad (tail edades)

------------------------------------------------------------------------------------------------

{-
18. Se tiene información de las lluvias en un determinado mes por Ej: se conoce la siguiente 
    información:
    lluviasEnero = [0,2,5,1,34,2,0,21,0,0,0,5,9,18,4,0]  
      a. (muy difícil) Definir la función rachasLluvia/1, que devuelve una lista de las listas 
         de los días seguidos que llovió. P.ej. se espera que la consulta 
         Main> rachasLluvia lluviasEnero 
         [[2,5,1,34,2],[21],[5,9,18,4]]. 
      b. A partir de esta definir mayorRachaDeLluvias/1, que devuelve la cantidad máxima de días 
         seguidos que llovió. P.ej. se espera que la consulta mayorRachaDeLluvias lluviasEnero 
         devuelva 5. 
         Ayuda: ver las funciones dropWhile y takeWhile, probar p.ej. esto:
         takeWhile even [2,4,7,10,14,15]
         dropWhile even [2,4,7,10,14,15] 
-}
lluviasEnero :: [Int]
lluviasEnero = [0,2,5,1,34,2,0,21,0,0,0,5,9,18,4,0]

-- a.

rachasLluvia :: [Int] -> [[Int]]
rachasLluvia []       = []
rachasLluvia (dia:dias)
   | dia == 0   = rachasLluvia dias
   | otherwise  = [] ++ takeWhile (> 0) (dia:dias) : rachasLluvia(dropWhile (> 0) dias)

-- b.

mayorRachaDeLluvias :: [[Int]] -> Int
mayorRachaDeLluvias = maximum . map length
------------------------------------------------------------------------------------------------

{-
19. Definir una función que sume una lista de números. 
    Nota: Resolverlo utilizando foldl/foldr. 
-}

sumarListas :: Num a => [a] -> a
sumarListas = foldl (+) 0

------------------------------------------------------------------------------------------------

{-
20. Definir una función que resuelva la productoria de una lista de números. 
    Nota: Resolverlo utilizando foldl/foldr. 
-}

productoListas :: Num a => [a] -> a
productoListas = foldr (*) 1

------------------------------------------------------------------------------------------------

{-
21. Definir la función dispersion, que recibe una lista de números y devuelve la dispersión de 
    los valores, o sea máximo - mínimo. 
    Nota: Probar de utilizar foldr.
-}

{-
dispersion :: [Int] -> Int
dispersion numeros = maximo numeros - minimo numeros
  where
   maximo = foldr1 (<)
   minimo = foldr1 (>)
   -}