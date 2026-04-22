import Text.Show.Functions
import Data.List(genericLength)

---------------------------------------- Punto 1 ----------------------------------------

data Auto = Auto{
    marca           :: String,
    modelo          :: String,
    desgaste        :: Desgaste,
    velocidadMaxima :: Float,
    tiempoDeCarrera :: Float
} deriving (Show)

type Desgaste = (Float,Float) -- El primer campo será el desgaste del chasis y el segundo el de las ruedas

ferrari = Auto{
    marca = "Ferrari",
    modelo = "F50",
    desgaste = (0,0),
    velocidadMaxima = 65,
    tiempoDeCarrera = 0
}

lamborghini = Auto{
    marca = "Lamborghini",
    modelo = "Diablo",
    desgaste = (7,4),
    velocidadMaxima = 73,
    tiempoDeCarrera = 0
}

fiat = Auto{
    marca = "Fiat",
    modelo = "600",
    desgaste = (33,27),
    velocidadMaxima = 44,
    tiempoDeCarrera = 0
}

---------------------------------------- Punto 2 ----------------------------------------

autoEnBuenEstado :: Auto -> Bool
autoEnBuenEstado unAuto = desgasteChasis unAuto < 40 && desgasteRuedas unAuto < 60

noDaMas :: Auto -> Bool
noDaMas unAuto = desgasteChasis unAuto > 80 || desgasteRuedas unAuto > 80

-- Auxiliar

desgasteChasis :: Auto -> Float
desgasteChasis = fst . desgaste

desgasteRuedas :: Auto -> Float
desgasteRuedas = snd . desgaste

---------------------------------------- Punto 3 ----------------------------------------

repararUnAuto :: Auto -> Auto
repararUnAuto =  cambiarRuedas . repararChasis

-- Auxiliar

repararChasis :: Auto -> Auto
repararChasis = alterarDesgasteChasis (*0.15)

cambiarRuedas ::  Auto -> Auto
cambiarRuedas = alterarDesgasteRuedas (*0)

alterarDesgasteChasis :: (Float -> Float) -> Auto -> Auto
alterarDesgasteChasis desgasteGenerado unAuto = unAuto { desgaste = (desgasteGenerado . desgasteChasis $ unAuto , desgasteRuedas unAuto) }

alterarDesgasteRuedas :: (Float -> Float) -> Auto -> Auto
alterarDesgasteRuedas desgasteGenerado unAuto = unAuto { desgaste = (desgasteChasis unAuto , desgasteGenerado . desgasteRuedas $ unAuto) }

---------------------------------------- Punto 4 ----------------------------------------

type PartePista = (Auto -> Auto)

-- a.

curva :: Float -> Float -> PartePista
curva angulo longitud = sumarTiempoCurva longitud . generarDesgasteRuedas longitud angulo

-- -- i.
curvaPeligrosa :: PartePista
curvaPeligrosa = curva 60 300

-- -- ii.
curvaTranca :: PartePista
curvaTranca = curva 110 550

-- Auxiliar

sumarTiempo :: (Auto -> Float) -> Auto -> Auto
sumarTiempo funcion unAuto = unAuto { tiempoDeCarrera = (+ tiempoDeCarrera unAuto) . funcion $ unAuto } 

sumarTiempoCurva :: Float -> Auto -> Auto
sumarTiempoCurva longitud unAuto = sumarTiempo ((longitud/) . (/2) . velocidadMaxima) unAuto

generarDesgasteRuedas :: Float -> Float -> Auto -> Auto
generarDesgasteRuedas longitud angulo = alterarDesgasteRuedas (+ (3 * longitud / angulo))

-- b.
tramoRecto :: Float -> PartePista
tramoRecto longitud = sumarTiempoRecta longitud . generarDesgasteChasis longitud

-- -- i.
tramoRectoClassic :: PartePista
tramoRectoClassic = tramoRecto 750

-- -- ii.
tramito :: PartePista
tramito = tramoRecto 280

-- Auxiliar

sumarTiempoRecta :: Float -> Auto -> Auto
sumarTiempoRecta longitud = sumarTiempo ((longitud/) . velocidadMaxima)

generarDesgasteChasis :: Float -> Auto -> Auto
generarDesgasteChasis longitud = alterarDesgasteChasis (longitud/100 +) 

-- c.
tramoBoxes :: Float -> PartePista
tramoBoxes longitud unAuto
    | autoEnBuenEstado unAuto = sumarTiempoRecta longitud unAuto
    | otherwise               = sumarTiempo ((+ 10) . tiempoDeCarrera) . sumarTiempoRecta longitud $ unAuto

-- d.
tramoMojado :: PartePista -> PartePista
tramoMojado tramoPista unAuto = sumarTiempo (diferenciaTiempo tramoPista) . tramoPista $ unAuto

diferenciaTiempo :: PartePista -> Auto -> Float
diferenciaTiempo unTramo unAuto = (/ 2) . ((tiempoDeCarrera . unTramo) unAuto -) . tiempoDeCarrera $ unAuto

-- e.
tramoConRipio :: PartePista -> PartePista
tramoConRipio tramoPista = tramoPista . tramoPista

-- f.
tramoObstruido :: Float -> PartePista -> PartePista
tramoObstruido longitud tramoPista = alterarDesgasteChasis (+ (2 * longitud)) . alterarDesgasteRuedas (+ (2 * longitud)) . tramoPista

---------------------------------------- Punto 5 ----------------------------------------

pasarPorTramo :: PartePista -> Auto -> Auto
pasarPorTramo unTramo unAuto
    | noDaMas unAuto = unAuto
    | otherwise      = unTramo unAuto

---------------------------------------- Punto 6 ----------------------------------------
type Pista = [PartePista]

-- a.
superPista :: Pista
superPista = [tramoRectoClassic, curvaTranca, tramoMojado tramito, tramito, tramoObstruido 2 (curva 80 400), curva 115 650, tramoRecto 970, curvaPeligrosa, tramoConRipio tramito, tramoBoxes 800]

-- b.
peganLaVuelta :: Pista -> [Auto] -> [Auto]
peganLaVuelta unaPista = filter noDaMas . pegaronLaVuelta unaPista

-- Auxiliar

pegaronLaVuelta :: Pista -> [Auto] -> [Auto]
pegaronLaVuelta unaPista []                          = []
pegaronLaVuelta unaPista (primerAuto:autosRestantes) = pasarPorPista unaPista primerAuto : pegaronLaVuelta unaPista (autosRestantes)

pasarPorPista :: Pista -> Auto -> Auto
pasarPorPista unaPista unAuto = foldr pasarPorTramo unAuto unaPista

---------------------------------------- Punto 7 ----------------------------------------

-- a.

data Carrera = Carrera{
    pista :: Pista,
    vueltas :: Int
}

-- b.

tourBuenosAires = Carrera{
    pista = superPista,
    vueltas = 20
}

-- c.
jugarUnaCarrera :: Carrera -> [Auto] -> [Auto]
jugarUnaCarrera (Carrera unaPista 1)           unosAutos = peganLaVuelta unaPista unosAutos
jugarUnaCarrera (Carrera unaPista unasVueltas) unosAutos = jugarUnaCarrera (Carrera unaPista (unasVueltas - 1)) . peganLaVuelta unaPista $ unosAutos