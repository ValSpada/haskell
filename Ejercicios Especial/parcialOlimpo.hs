-------------
---PUNTO 1---
-------------

data Heroe = Heroe{
    epiteto        :: String,
    reconocimiento :: Int,
    artefactos     :: [Artefacto],
    tareas         :: [Tarea]
}deriving (Show)

data Artefacto = Artefacto{
    nombre :: String,
    rareza :: Int
}deriving (Show)

---PUNTO 2---
-------------

pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria unHeroe
    | reconocimiento unHeroe > 1000 = cambiarEpiteto "El mítico" unHeroe 
    | reconocimiento unHeroe >= 500 = cambiarEpiteto "El magnífico" . agregarArtefacto lanzaDeOlimpo $ unHeroe
    | reconocimiento unHeroe >  100 = cambiarEpiteto "Hoplita" . agregarArtefacto xiphos $ unHeroe
    | otherwise                     = unHeroe

lanzaDeOlimpo :: Artefacto
lanzaDeOlimpo = Artefacto "Lanza del Olimpo" 100

xiphos :: Artefacto
xiphos = Artefacto "Xiphos" 50

cambiarEpiteto :: String -> Heroe -> Heroe
cambiarEpiteto unEpiteto unHeroe = unHeroe {epiteto = unEpiteto}

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto unArtefacto unHeroe = cambiarArtefactos (unArtefacto :)

-------------
---PUNTO 3---
-------------

-- Cohesión -> La función haga solo lo que debe hacer y no más, si se desprende otra lógica se usa otra función
-- Declaratividad
-- Expresividad
-- o&oo -> Once and only once

type Tarea = Heroe -> Heroe

encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto unArtefacto = ganarReconomiento (rareza unArtefacto) . agregarArtefacto unArtefacto

escalarElOlimpo :: Tarea
escalarElOlimpo = agregarArtefacto relampagoDeZeus . desecharArtefactosComunes . triplicarRarezaArtefactos . ganarReconomiento 500

ayudarACruzarCalle :: Int -> Tarea
ayudarACruzarCalle cantidadCuadras = cambiarEpiteto ("Gros" ++ replicate 'o' cantidadCuadras)

matarUnaBestia :: Bestia -> Tarea
matarUnaBestia matarUnaBestia unHeroe
    | debilidad matarUnaBestia unHeroe = cambiarEpiteto ("El asesino de " ++ nombreBestia matarUnaBestia) unHeroe
    | otherwise                        = cambiarEpiteto "El cobarde" . cambiarArtefactos (drop 1) $ unHeroe

-- Auxiliar

ganarReconomiento :: Int -> Heroe -> Heroe
ganarReconomiento aumento unHeroe = unHeroe {reconocimiento = aumento + reconocimiento unHeroe}

triplicarRarezaArtefactos :: Tarea 
triplicarRarezaArtefactos = cambiarArtefactos (map triplicarRarezaArtefacto)

triplicarRarezaArtefacto :: Artefacto -> Artefacto
triplicarRarezaArtefacto unArtefacto = unArtefacto {rareza = (3*) . rareza $ artefactos}

desecharArtefactosComunes :: Tarea
desecharArtefactosComunes = cambiarArtefactos (filter (not . esComun))

esComun :: Artefacto -> Bool
esComun = (< 1000) . rareza

cambiarArtefactos :: ([Artefacto] -> [Artefactos]) -> Heroe -> Heroe
cambiarArtefactos modificador unHeroe = unHeroe {artefactos = modificador (artefactos unHeroe)}

data Bestia = Bestia{
    nombreBestia :: String,
    debilidad    :: Debilidad
}deriving(Show)

type Debilidad = Heroe -> Bool

relampagoDeZeus :: Artefacto
relampagoDeZeus = Artefacto "Relampago de Zeus" 500

-------------
---PUNTO 4 y 5---
-------------
heracles :: Heroe
heracles = Heroe "Guardián del Olimpo" 700 [pistolaRara, relampagoDeZeus] [matarUnaBestia leonDeNemea]

pistolaRara :: Artefacto
pistolaRara = Artefacto "Fierro de la antigua Grecia" 1000

-------------
---PUNTO 5---
-------------

leonDeNemea :: Bestia
leonDeNemea = Bestia "León de Nemea" ((> 20) . length . epiteto)

-------------
-- PUNTO 6 --
-------------

hacerUnaTarea :: Tarea -> Heroe -> Heroe
hacerUnaTarea unaTarea = agregarTarea unaTarea . unaTarea

agregarTarea :: Tarea -> Heroe -> Heroe
agregarArtefacto unaTarea unHeroe = unHeroe {tareas = unaTarea : tareas unHeroe}

-------------
-- PUNTO 7 --
-------------

presumir :: Heroe -> Heroe -> (Heroe, Heroe)
presumir heroe1 heroe2
    | gana heroe1 heroe2 = (heroe1:heroe2)
    | gana heroe2 heroe1 = (heroe2:heroe1)
    | otherwise          = presumir (realizarTareasDe heroe1 heroe2) (realizarTareasDe heroe2 heroe1)

realizarTareasDe :: Heroe -> Heroe -> Heroe
realizarTareasDe unHeroe otroHeroe = realizarLabor (tareas otroHeroe) unHeroe

gana :: Heroe -> Heroe -> Bool
gana ganador perdedor = reconocimiento unHeroe > reconocimiento otroHeroe || reconocimiento ganador == reconocimiento perdedor && sumatoriaRarezas unHeroe > sumatoriaRarezas otroHeroe

sumatoriaRarezas :: Heroe -> Int
sumatoriaRarezas = sum . map rarezas . artefactos 

-------------
-- PUNTO 8 --
-------------

-- CICLA INFINITAMENTE YA QUE NO HABRÁ NADA PARA APLICAR DE LAS TAREAS

-------------
-- PUNTO 9 --
-------------

realizarLabor :: [Tareas] -> Heroe -> Heroe
realizarLabor unasTareas unHeroe = foldl (flip hacerUnaTarea) unHeroe unasTareas

-- FOLDL -> Primero al ultimo

--------------
-- PUNTO 10 --
--------------

-- No terminaria nunca