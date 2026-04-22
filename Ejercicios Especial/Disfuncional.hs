-- tieneNombreLargo mascota = length (fst mascota) > 9 == True
-- ¿PORQUE COMPARA SI EL RESULTADO ES TRUE SI YA COMPARAS CON EL > Y LA FUNCION DA EL RESULTADO?

-- CORREGIDO:
tieneNombreLargo :: (String,b) -> Bool
tieneNombreLargo mascota = length (fst mascota) > 9

-------------------------------------------------------------------------------------

-- sumarEnergia (Persona _ energia _ _) = (Persona _ (energia + 5) _ _)
-- Cuando queres devolver el constructor entero, debes recibir todos sus parametros y ademas no
-- se deben usar los "_" del lado derecho del "=" ya que es para indicar que no nos importa que recibimos

data Persona = Persona {
    nombre :: String,
    energia :: Int,
    dni :: Int,
    edad :: Int
} deriving (Show)

alfredo = Persona{
    nombre = "Alfredo",
    energia = 2,
    dni = 2130123,
    edad = 0
}

-- CORREGIDO:
sumarEnergia persona = persona {energia = energia persona + 5 }

-------------------------------------------------------------------------------------

-- triplicarLosPares numeros = (map (*3) . filter . even) numeros
-- Esto daria error porque primero verifica que numeros son pares y despues los va a filtrar pero
-- no tiene condicion para filtrar

triplicarLosPares numeros = (map (*3) . filter even) numeros

-------------------------------------------------------------------------------------

-- sonTodosMamiferos animales = all (map esMamifero animales) animales
-- No es logico el primer parametro, deberia ser la condicion (esMamiero) y pasarle solo animales

-- CORRECCION -> all esMamifero animales

-- sonTodosMamiferos animales = (and . map esMamifero) animales
-- Funciona pero sería mejor hacerlo con all ya que el and y map son justamente el all

-- CORRECCION -> all esMamifero animales

-------------------------------------------------------------------------------------

{-
abrirVentanas :: Casa -> Casa
prenderEstufa :: Casa -> Casa
encenderElAireA :: Casa -> Int -> Casa
mudarseA :: String -> Casa -> Casa
miCasaINteligente = CAsa{
    direccion="Medrano 951",
    temperatura=26,
    reguladores = [abrirVentanas,prenderEstufa,mudarseA ,encenderElAireA 24]
    }
-}

-- Las funciones no son del mismo tipo, para las listas es necesario que sean del mismo tipo

abrirVentanas :: Casa -> Casa
prenderEstufa :: Casa -> Casa
encenderElAireA :: Int -> Casa -> Casa
mudarseA :: String -> Casa -> Casa
miCasaINteligente = CAsa{
    direccion="Medrano 951",
    temperatura=26,
    reguladores = [abrirVentanas,prenderEstufa,mudarseA "Enfrente",encenderElAireA 24]
    }

-------------------------------------------------------------------------------------

-- esBeatle _ = False
-- esBeatle "Ringo" = True
-- esBeatle "John" = True
-- esBeatle "George" = True
-- esBeatle "Paul" = True

-- Con todo respeto, es medio boludo. Definio mal el pattern matching

-- CORRECCION
esBeatle "Ringo" = True
esBeatle "John" = True
esBeatle "George" = True
esBeatle "Paul" = True
esBeatle _ = False

-------------------------------------------------------------------------------------

{-
sumaDeLasEdadesRecursiva [] = 0
sumaDeLasEdadesRecursiva lista = edad (head lista) + sumaDeLasEdadesREcursiva (drop 1 lista)
-}
-- La funcion edad no sirve de nada, si ya la lista son las edades

--CORRECCION
sumaDeLasEdadesRecursiva [] = 0
sumaDeLasEdadesRecursiva (persona:personas) = edad persona + sumaDeLasEdadesRecursiva personas

sumaDeLasEdadesRecursiva' = sum . map edad

-------------------------------------------------------------------------------------

{-
abrirVentanas casa = casa{
    direccion = direccion casa
    temperatura = temperatura casa - 2,
    reguladoresDeTemperatura = reguladoresDeTemperatura casa
}
-}

abrirVentanas casa = casa{
    temperatura = temperatura casa - 2
}

-- funcion temperatura, toma la casa y devuelve la temperatura de la casa actual

-------------------------------------------------------------------------------------

{-
    agregarValor valor indice lista = take (indice-1) lista ++ [valor] ++ drop indice lista
-}

agregarValor valor indice lista = take (indice-1) lista ++ valor : drop indice lista

-------------------------------------------------------------------------------------

-- poneleUnNombre numeros = (sum (map (*3) (filter even numeros))) < 100
poneleUnNombre numeros = (< 100) . sum . map (*3) . filter even -- COMPOSICION
