data Sustancia =
                Sencillo {
                    nombre :: String,
                    simbolo :: String,
                    nAtomico :: Int,
                    familia :: Familia }
              | Compuesto {
                    nombre :: String,
                    componente :: [Componente],
                    familia :: Familia
                } deriving (Show, Eq)

data Familia = Metal | NoMetal | Halogeno | GasNoble deriving (Show, Eq)

data Componente = Componente {
                    sustancia :: Sustancia,
                    cantMoleculas :: Int
                    } deriving (Show, Eq)


-- Punto 1: Creacion de datas
hidrogeno :: Sustancia
hidrogeno = Sencillo {
    nombre = "Hidrogeno",
    simbolo = "H",
    nAtomico = 1,
    familia = NoMetal
}

oxigeno :: Sustancia
oxigeno = Sencillo {
    nombre = "Oxigeno",
    simbolo = "O",
    nAtomico = 8,
    familia = NoMetal
}

agua :: Sustancia
agua = Compuesto {
    nombre = "Agua",
    componente = [Componente hidrogeno 2,Componente oxigeno 1],
    familia = NoMetal
}

--Punto 2: Criterios de eñectrocodad y calor. 

data Conductor = Calor | Electricidad 

conduce :: Conductor -> Sustancia -> Bool
conduce _ (Sencillo _ _ _ Metal) = True
conduce _ (Compuesto _ _  Metal) = True
conduce Electricidad (Sencillo _ _ _ GasNoble) = True
conduce Calor (Compuesto _ _  Halogeno) = True
conduce _ _ = False


--Punto 3: Modifical un nombre, agregar "uro" luego de la ultima consonante
esVocal :: Char -> Bool
esVocal letra = elem letra ['A','E','I','O','U','a','e','i','o','u']

cortarVocales :: String -> String
cortarVocales nombre = reverse. dropWhile esVocal . reverse $ nombre

modificarNombre :: String -> String
modificarNombre nombre = cortarVocales nombre ++ "uro"

-- Punto 4: Combinacion de dos nombres. "NombreDeUnion" ++ " de " ++ "SegundoNombre"

unionNombres :: String -> String -> String
unionNombres nombre1 nombre2 = modificarNombre nombre1 ++ " de " ++ nombre2

-- Punto 5: Mezcla de componentes, obteniendo un compuesto.

darNombreComponente :: Componente -> String
darNombreComponente componente = (nombre.sustancia)componente

crearListaNombres :: [Componente] -> [String]
crearListaNombres componentes = map darNombreComponente componentes 
{-
unionDeNombres :: [Componente] -> String
unionDeNombres componente = unionNombres darNombreComponente(head componente)  darNombreComponente(tail componentes)
 
crearCompuesto :: [Componente] -> Sustancia
crearCompuesto componentes = Compuesto {
                                        nombre = unionNombres()

                                        familia = NoMetal
                                    }-}

-- Punto 6
