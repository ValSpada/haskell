import Data.List

----------------------------------------------------------------------------------------------

{-
1. Definir una funcion que sume una lista de números.
   Nota: Investigar sum
-}
sumarListaNumeros :: Num a => [a] -> a
sumarListaNumeros = sum

----------------------------------------------------------------------------------------------

{-
2. Durante un entrenamiento fisico de una hora, cada 10 minutos de entrenamiento se
   tomó la frecuencia total de 7 muestras que son las siguientes:
   frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]
   Comienza con una frecuencia de 80 min 0.
   A los 10 min la frecuencia alcanza los 100
   A los 20 min la frecuencia es de 120.
   A los 30 min la frecuencia es de 128.
   A los 40 min la frecuencia es de 130, etc...
   A los 60 min la frecuencia es de 125, frecuenciaCardiaca es una funcion constante.
    a. Definir la función promedioFrecuenciaCardiaca, que devuelve el promedio de la
       frecuencia cardiaca.
       Main> promedioFrecuenciaCardiaca
       115.285714285714
    b. Definir la función frecuenciaCardiacaMinuto/1, que recibe m que es el minuto
       en el cual quiero conocer la frecuencia cardiaca, m puede ser a los 10, 20, 30,
       40, hasta 60.
       Main> frecuenciaCardiacaMinuto 30
       128
       Ayuda: Vale definir una función auxiliar para conocer el número de muestra.
    c. Definir la función frecuenciasHastaMomento/1, devuelve el total que se obtuvieron
       hasta el minuto m.
       Main> frecuenciasHastaMomento 30
       [80, 100, 120, 128]
       Ayuda: Utilizar la función take y la funcion auxiliar definida en el punto anterior.
-}
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]

-- a.
promedioFrecuenciaCardiaca :: Double
promedioFrecuenciaCardiaca = calcularPromedio frecuenciaCardiaca

-- b.
frecuenciaCardiacaMinuto :: Int -> Double
frecuenciaCardiacaMinuto minuto = frecuenciaCardiaca !! (obtenerNumeroMuestra minuto)

-- c.
frecuenciasHastaMomento :: Int -> [Double]
frecuenciasHastaMomento minuto = take (obtenerFrecuencias minuto) frecuenciaCardiaca

-- Auxiliar
calcularPromedio :: Fractional a => [a] -> a
calcularPromedio lista = sum lista / Data.List.genericLength lista

obtenerNumeroMuestra :: Int -> Int
obtenerNumeroMuestra numero = div numero 10

obtenerFrecuencias :: Int -> Int
obtenerFrecuencias = (+ 1) . obtenerNumeroMuestra

----------------------------------------------------------------------------------------------

{-
3. Definir la función esCapicua/1, si data una lista de listas, me devuelve si la concatenación 
   de las sublistas es una lista capicua. Ejemplo
   Main> esCapicua ["ne", "uqu", "en"]
   True
   Porque "neuquen" es capicua.
   Ayuda: Utilizar concat/1, reverse/1
-}

esCapicua :: [[Char]] -> Bool
esCapicua lista = (invertirCadena lista) == (concat lista)

-- Auxiliar
invertirCadena :: [[Char]] -> [Char]
invertirCadena = reverse . concat

----------------------------------------------------------------------------------------------

{-
4. Se tiene información detallada de la duración en minutos de las llamadas que se llevaron a
   cabo en un período determinado, discriminadas en horario normal y horario reducido.
   duracionLlamadas = (("horarioReducido",[20,10,25,15]),("horarioNormal",[10,5,8,2,9,10]))
    a. Definir la función cuandoHabloMasMinutos, devuelve en que horario se habló más cantidad
       de minutos, en el de tarifa normal o en el reducido.
       *Main> cuandoHabloMasMinutos
       "horarioReducido"
    b. Definir la función cuandoHizoMasLlamadas, devuelve el que franja horaria realizó más
       cantidad de llamadas, en el de tarifa normal o en el reducido.
       *Main> cuandoHizoMasLlamadas
       "horarioNormal"
   Nota: Utilizar composición en ambos casos
-}
duracionLlamadas :: ((String,[Int]),(String,[Int]))
duracionLlamadas = (("horarioReducido",[20,10,25,15]),("horarioNormal",[10,5,8,2,9,10]))

-- Punto a.
cuandoHabloMasMinutos :: String
cuandoHabloMasMinutos
   | (sum . snd . fst) duracionLlamadas > (sum . snd . snd) duracionLlamadas = (fst . fst) duracionLlamadas
   | otherwise                                                               = (fst . snd) duracionLlamadas

-- Punto b.
cuandoHizoMasLlamadas :: String
cuandoHizoMasLlamadas
   | (length . snd . fst) duracionLlamadas > (length . snd . snd) duracionLlamadas = (fst . fst) duracionLlamadas
   | otherwise                                                                     = (fst . snd) duracionLlamadas