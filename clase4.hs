import Data.List
import Text.Show.Functions

----------------------------------------- ESTRUCTURAS -----------------------------------------
-- Creamos nuevos tipos de datos

-- data Pokemon = Pokemon String String [String]
-- data   tipo  = constructor/deconstructor tipo de var
--       EN MAYUSCULAS
-- Los constructores serán pokemones 

-- NOTACION LLAVES
data Pokemon = Pokemon {
    nombre :: String,
    familia :: String,
    tipoPokemon :: TiposPokemon
} deriving (Show)-- deriving (Eq) | Todo miembro interno tiene que pertenecer a Eq
-- NO SE PUEDEN REPETIR "NOMBRES" EN DATAS

-- enum = tambien datas | MAS DE UN CONSTRUCTOR | PERMITIMOS QUE EL CONSTRUCTOR SEA COMPARABLE
data Elemento = Fuego | Psiquico | Acero deriving (Eq, Show)
data DiaDeLaSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
-- Podemos hacer que caulquier tipo pertenezca a cualquier Clase/Familia/Restriccion
-- mas custom

data TiposPokemon = Unico Elemento | Doble Elemento Elemento deriving (Show)

primario (Unico elemento) = elemento
primario (Doble elemento1 _) = elemento1
secundario (Doble _ elemento2) = elemento2

-- _ -> VARIABLE ANONIMA | NO SE DEVUELVE, ES BASURA PERO TIENE QUE RECIBIR ALGO

-- POR CONVENCION PARA UN ELEMENTO, PRIMERA OPCION. PARA VARIOS, VARIOS CONSTRUCTORES

-- SI LE PONEMOS DERIVING, COMPARA TODO DE UN POKEMON

charmander :: Pokemon
-- charmander = Pokemon "Charmander" "Salamandra piola" ["Fuego"]
-- :t Pokemon
-- Pokemon :: String -> String -> [String] -> Pokemon
-- Se puede aplicar parcialmente las estructuras
charmander = Pokemon {
    nombre = "Charmander",
    familia = "Salamandra piola",
    tipoPokemon = Unico Fuego
}

victini :: Pokemon
victini = Pokemon "Victini" "Pokemon PEronistas" (Doble Fuego Psiquico)

metagross :: Pokemon
metagross = Pokemon "Metagross" "Nunca saltea dia de pierna" (Doble Acero Psiquico)

pokedexDeAle :: [Pokemon]
pokedexDeAle = [charmander, victini, metagross]

-- NO existe el IF en Haskell, existen las funciones partidas
abs' x
    | x >= 0 =  x
    | x < 0  = -x
-- funcion parametro
--      | true = funcion1
--      | true = funcion2

signo :: Int -> Int
signo x
    | x > 0     = 1
    | x < 0     = -1
    | otherwise = 0
-- :t signo
-- signo :: (Ord a, Num a, Num p) => a -> p
-- DEBEN DEVOLVER EL MISMO TIPO DE DATO
-- OTHERWISE -> De otra forma y siempre es true
-- La prioridad es de arriba para abajo


evaluarPokedex :: [Pokemon] -> String
evaluarPokedex unaPokedex
    | cantidadPokemones <  50 = "Segui participando"
    | cantidadPokemones < 100 = "Venis bien pa!"
    | cantidadPokemones < 150 = "AHHHHHHH TE FALTA UNO"
    | otherwise               = "Completaste la pokedex, ya podes morir en paz"
    where 
        cantidadPokemones = length unaPokedex

-- Como va comparando cada caso, es redundante sumar comparaciones
-- REPETIR LOGICA, TRATAR DE NO HACERLO

familiasPokedex :: [Pokemon] -> [String]
familiasPokedex pokedex = map (familiaDe) pokedex

--            TIPO
familiaDe :: Pokemon -> String
familiaDe (Pokemon _ familia _) = familia 
--        Deconstructor
-- COLOCAR TODOS LOS PARAMETROS Y EN ORDEN

sonCompatibles :: Pokemon -> Pokemon -> Bool
sonCompatibles pokemon1 pokemon2 = 
    not . null . (intersect (tipoPokemon pokemon1)) $ (tipoPokemon pokemon2)
-- TAREA CORREGIR Y HACER QUE ANDE

-- MODELADO DE DATO - APUNTE

--------- PATTERN MATCHING ---------

-- EJEMPLO: Primario y Secundario para los data y tipos primitivos de haskell
esVocal letra = elem letra "aeiou"

-- USANDO PATTERN MATCHING:
esVocal 'a' = True
esVocal 'e' = True
esVocal 'i' = True
esVocal 'o' = True
esVocal 'u' = True
esVocal  _  = False
-- NON-EXHAUSTIVE PATTERNS -> error
-- Cuando defino por pattern matching, va en orden descendente y se queda con la primera
-- que le sigue
-- El patron mas generico va para el fondo

-- not True = False
-- not _    = True

-- [1,2,3]
-- 1:[2,3] o 1:2:[3] o 1:2:3:[]

-- head (x:_) = x 
-- tail (_:xs) = xs -> Al menos le debe llegar una cabeza
-- head [] = ...
-- quinto (_:_:_:_:x5:_) = x5 -> Lista de al menos 5 elementos

-- : -> Constructor de listas