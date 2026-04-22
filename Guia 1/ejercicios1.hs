{-
1. Definir la función esMultiploDeTres/1, que devuelve True si un número es
   múltiplo de 3, p.ej:
   Main> esMultiploDeTres 9
   True
-}

esMultiploDeTres :: Int -> Bool
esMultiploDeTres numero = mod numero 3 == 0

-- La funcion "mod" nos permite calcular el resto de dividir el primer numero
-- que pasemos como parametro con el segundo. Por ejemplo: mod 3 2, nos deberia
-- dar 1 como resultado.

-------------------------------------------------------------------------------------


{-
2. Definir la función esMultiploDe/2, que devuelve True si el segundo es
   múltiplo del primero, p.ej:
   Main> esMultiploDe 3 12
   True
-}

esMultiploDe :: Int -> Int -> Bool
esMultiploDe posibleMultiplo numeroBase = mod posibleMultiplo numeroBase == 0

-------------------------------------------------------------------------------------

{-
3. Definir la función cubo/1, que devuelve el cubo de un número.
-}

cubo :: Num a => a -> a
cubo numero = numero * numero * numero

-------------------------------------------------------------------------------------

{-
4. Definir la función area/2, devuelve el área de un rectángulo
   a partir de su base y su altura.
-}

area :: Num a => a -> a -> a
area base altura = base * altura

-------------------------------------------------------------------------------------

{-
5. Definir la función esBisiesto/1, indica si un año es bisiesto. (Un año es bisiesto
   si es divisible por 400 o es divisible por 4 pero no es divisible por 100) Nota:
   Resolverlo utilizando la funcion esMultiploDe/2
-}

esBisiesto :: Int -> Bool
-- esBisiesto año = esMultiploDe año 4 && (mod año 100 /= 0 || esMultiploDe año 400)
esBisiesto año = esMultiploDe año 4 && (not (esMultiploDe año 100) || esMultiploDe año 400)

-------------------------------------------------------------------------------------

{-
6. Definir la función celsiusToFahr/1, pasa una temperatura en grados Celsius a grados
   Fahrenheit.
-}

celsiusToFahr :: Fractional a => a -> a
celsiusToFahr temperaturaCelsius = (temperaturaCelsius * 9/5) + 32

-------------------------------------------------------------------------------------

{-
7. Definir la función fahrToCelsius/1, la inversa de la anterior.
-}

fahrToCelsius :: Fractional a => a -> a
fahrToCelsius temperaturaFahrenheit = (temperaturaFahrenheit - 32) * 5/9

-------------------------------------------------------------------------------------

{-
8. Definir la funciíon haceFrioF/1, indica si una temperatura expresada en grados
   Fahrenheit es fría. Decimos que hace frío si la temperatura es menor a 8 grados
   Celsius.
-}

haceFrioF :: Ord a => Fractional a => a -> Bool
haceFrioF temperaturaFahrenheit = fahrToCelsius temperaturaFahrenheit < 8;

-------------------------------------------------------------------------------------

{-
9. Definir la función mcm/2 que devuelva el mínimo común múltiplo entre dos números
   de acuerdo a esta fórmula.
   m.c.m.(a, b) = (a * b) / (m.c.d.(a, b))
-}

mcm :: Integral a => a -> a -> a
mcm numeroUno numeroDos = div (numeroUno * numeroDos) (gcd numeroUno numeroDos)

-- La función div funciona con la parte entera de la misma, opera con Integer.
-- La función gcd devuelve el mínimo común divisor entre dos números.

-------------------------------------------------------------------------------------

{-
10. Dispersión
        Trabajamos con tres números que imaginamos como el nivel del río Paraná a
        la altura de Corrientes medido en tres días consecutivos; cada medición es
        un entero que representa una cantidad de cm.
        P.ej. medí los días 1, 2 y 3, las mediciones son: 332 cm, 283 cm y 294 cm.
        A partir de estos tres números, podemos obtener algunas conclusiones.
        Definir estas funciones:

    a.  dispersion, que toma los tres valores y devuelve la diferencia entre el más
        alto y el más bajo. Ayuda: extender max y min a tres argumentos, usando las
        versiones de dos elementos. DE esa forma se puede definir dispersión sin
        escribir ninguna guarda (las guardas están en max y min, que estamos usando).

    b.  diasParejos, diasLocos y diasNormales reciben los valores de los tres días.
        Se dice que son días parejos si la dispersión es chica, que son dias locos
        si la dispersión es grande, y que son días normales si no son ni parejos ni
        locos. Una dispersión se considera chica si es de menos de 30 cm, y grande
        si es de más de un metro.
        Nota: DEfinir diasNormales a partir de las otras dos, no volver a hacer las
        cuentas-    
-}

--Auxiliar

-- El tipo de max y de min es Ord
maxTresNumeros :: Integral a => a -> a -> a -> a
maxTresNumeros numeroUno numeroDos numeroTres = max numeroUno (max numeroDos numeroTres)

minTresNumeros :: Integral a => a -> a -> a -> a
minTresNumeros numeroUno numeroDos numeroTres = min numeroUno (min numeroDos numeroTres)

esChica :: Integral a => a -> Bool
esChica diferencia = diferencia < 30

esGrande :: Integral a => a -> Bool
esGrande diferencia = diferencia > 100

-- Punto a.
dispersion :: Integral a => a -> a -> a -> a
dispersion primerDia segundoDia tercerDia = (maxTresNumeros primerDia segundoDia tercerDia) - (minTresNumeros primerDia segundoDia tercerDia)

-- Punto b.
-- diasParejos :: Integral a => a -> Bool
-- diasParejos diferencia = diferencia < 30
diasParejos :: Integral a => a -> a -> a -> Bool
diasParejos dia1 dia2 dia3 = esChica $ dispersion dia1 dia2 dia3

-- diasLocos :: Integral a => a -> Bool
-- diasLocos diferencia = diferencia > 100
diasLocos :: Integral a => a -> a -> a -> Bool
diasLocos dia1 dia2 dia3 = esGrande $ dispersion dia1 dia2 dia3

diasNormales :: Integral a => a -> a -> a -> Bool
diasNormales dia1 dia2 dia3 = not (diasParejos dia1 dia2 dia3) && not (diasLocos dia1 dia2 dia3)

-------------------------------------------------------------------------------------

{-
11. En una plantación de pinos, de cada árbol se conoce la altura expresada en cm. El
    peso de un pino se puede calcular a partir de la altura así: 3 kg * cm hasta 3 metros,
    2 kg * cm arriba de los 3 metros. P.ej. 2 metros => 600 kg, 5 metros => 1300 kg.
    Los pinos se usan apra llevarlos a una fábrica de muebles, a la que le sirven árboles
    de entre 400 y 1000 kilos, un pino fuera de este rango no le sirve a la fábrica. Para
    esta situación:
        a. Definir la función pesoPino, recibe la altura de un pino y devuelve su peso.
        b. Definir la función esPesoUtil, recibe un peso en kg y devuelve True si un
           pino de ese peso le sirve a la fábrica, y False en caso contrario.
        c. Definir la función sirvePino, recibe la altura de un pino y devuelve True si
           un pino de ese peso le sirve a la fábrica, y False en caso contrario. Usar
           composición en la definición.
-}

-- Auxiliar
pesoPinoChico :: Integral a => a -> a
pesoPinoChico altura = altura * 3

pesoPinoGrande :: Integral a => a -> a
pesoPinoGrande altura = altura * 2

-- Punto a.
pesoPino :: Integral a => a -> a
-- pesoPino altura | altura <= 300 = (pesoPinoChico altura)
--                 | altura > 300 = (pesoPinoGrande altura)
pesoPino altura = pesoPinoGrande (max (altura - 300) 0) + pesoPinoChico (min altura 300)

-- Punto b.
esPesoUtil :: Integral a => a -> Bool
esPesoUtil peso = peso >= 400 && peso <= 1000

-- Punto c.
sirvePino :: Integral a => a -> Bool
sirvePino altura = esPesoUtil (pesoPino altura)

-------------------------------------------------------------------------------------

{-
12. Este ejercicio alguna vez se planteó como un Desafío Café con Leche: Implementar
    la función esCuadradoPerfecto/1, sin hacer operaciones con punto flotante.
    Ayuda: les va a venir bien una función auxiliar, tal vez de dos parámetros. Pensar
    que el primer cuadrado perfecto es 0, para llegar al 2do (1) sumo 1, para llegar al
    3ro (4) sumo 3, para llegar al siguiente (9) sumo 5, despues sumo 7, 9, 11, etc...
    También algo de recursividad van a tener que usar.
-}

-- Calcular la raiz cuadrada del numero que le pasemos y ver si su resultado es redondo.
-- No se puede dividir ni usar la raiz, todo a mano.

esCuadradoPerfecto :: Integral a => a -> Bool
esCuadradoPerfecto numero = calcularCuadradoPerfecto numero 0  

-- Auxiliar
calcularCuadradoPerfecto :: Integral a => a -> a -> Bool
-- calcularCuadradoPerfecto numero cantidad | cantidad ^ 2 == numero = True
--                                          | cantidad ^ 2 <  numero = (calcularCuadradoPerfecto numero (cantidad+1))
--                                          | cantidad ^ 2 >  numero = False
calcularCuadradoPerfecto numero cantidad = (not $ cantidad ^ 2 > numero) && (cantidad ^ 2 == numero || calcularCuadradoPerfecto numero (cantidad + 1))