import Text.Show.Functions
import Data.List

-- Funciones que tal vez te pueden servir, tal vez no

-- Main*> :t takeWhile
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- Main*> takeWhile even [2,4,6,5,6,7,8,9]
-- [2,4,6]

-- Main*> :t genericLength
-- genericLength :: Num i => [a] -> i
-- Main*> genericLength [2,4,6,5,6,7,8,9]
-- 8

-------------------------------- Anotaciones Personales --------------------------------

{-
1) Para funciones que retornen booleanos, nombrarlas como preguntas.
2) Para funciones que retornan un elemento, nombrarlas con sustantivos.
3) No usar sinonimos en funciones que actuan distinto.
4) Aprovecharme del uso de funciones para listas para no hardcodear como un mono.
5) Cuando no me matcheen los tipos, delegar en funciones donde sí y después evaluo.
6) Leer bien los errores y no tener ansiedad.
7) LEER TODO EL PARCIAL !!!!!!!!
8) Aprovecharme los alias de tipo para ganar expresividad y evaluar bien como modelar.
-}

--------------------------------         Punto 1        --------------------------------

data GuerreroZ = GuerreroZ{
    nombre       :: String,
    ki           :: Float, 
    raza         :: Raza,
    cansancio    :: Float,
    personalidad :: Personalidad
} deriving (Show,Eq)

data Raza = Humano | Namekiano | Saiyajin deriving (Show,Eq)

data Personalidad = Sacado | Perezoso | Tramposo deriving (Show,Eq)

gohan = GuerreroZ "Gohan" 10000 Saiyajin 0 Perezoso
yajirobe = GuerreroZ "Yajirobe" 875 Humano 0 Tramposo
vegeta = GuerreroZ "Vegeta" 22000 Saiyajin 0 Sacado

--------------------------------         Punto 2        --------------------------------

esPoderoso :: GuerreroZ -> Bool
esPoderoso unGuerrero = ki unGuerrero > 8000 || raza unGuerrero == Saiyajin

--------------------------------         Punto 3        --------------------------------

type Ejercicio  = GuerreroZ -> GuerreroZ

pressBanca :: Ejercicio
pressBanca = alterarGains (+ 90) . alterarFatiga (+ 100)

flexionesDeBrazo :: Ejercicio
flexionesDeBrazo = id . alterarFatiga (+ 50)

saltosAlCajon :: Float -> Ejercicio
saltosAlCajon centimetros = alterarGains (+ centimetros/10) . alterarFatiga (+ centimetros/5)

snatch :: Ejercicio
snatch unGuerrero 
    | esExperimentado unGuerrero = alterarGains (* 1.05) . alterarFatiga (* 1.1) $ unGuerrero
    | otherwise                  = alterarFatiga (+ 100) $ unGuerrero

estaCansado :: GuerreroZ -> Bool
estaCansado unGuerrero = cualEsSuEstado unGuerrero 0.44

estaExhausto :: GuerreroZ -> Bool
estaExhausto unGuerrero = cualEsSuEstado unGuerrero 0.72

realizarUnEjercicio :: Ejercicio -> GuerreroZ -> GuerreroZ
realizarUnEjercicio unEjercicio unGuerrero
    | estaCansado unGuerrero  = alterarGains (+ diferenciaGains * 2) . alterarFatiga (+ diferenciaFatiga * 4) $ unGuerrero
    | estaExhausto unGuerrero = alterarGains (* 0.98) unGuerrero
    | otherwise               = unEjercicio unGuerrero
    where 
        diferenciaFatiga = (cansancio . unEjercicio $ unGuerrero) - cansancio unGuerrero
        diferenciaGains  = (ki . unEjercicio $ unGuerrero) - ki unGuerrero

-- Auxiliar
alterarFatiga :: (Float -> Float) -> GuerreroZ -> GuerreroZ
alterarFatiga fatigaGenerada unGuerrero = asignarCansancio (fatigaGenerada . cansancio $ unGuerrero) unGuerrero

alterarGains :: (Float -> Float) -> GuerreroZ -> GuerreroZ
alterarGains gainsGeneradas unGuerrero = asignarKi (gainsGeneradas . ki $ unGuerrero) unGuerrero

asignarCansancio :: Float -> GuerreroZ -> GuerreroZ
asignarCansancio unCansancio unGuerrero = unGuerrero{ cansancio = max 0 unCansancio }

asignarKi :: Float -> GuerreroZ -> GuerreroZ
asignarKi unKi unGuerrero = unGuerrero{ ki = unKi }

esExperimentado :: GuerreroZ -> Bool
esExperimentado = (>= 22000) . ki

cualEsSuEstado :: GuerreroZ -> Float -> Bool
cualEsSuEstado (GuerreroZ _ unKi _ unCansancio _) valorAEvaluar = unCansancio > (valorAEvaluar * unKi)

--------------------------------         Punto 4        --------------------------------

armarRutina :: GuerreroZ -> [Ejercicio] -> [Ejercicio]
armarRutina (GuerreroZ _ _ _ _ Sacado)   unaRutina = unaRutina
armarRutina (GuerreroZ _ _ _ _ Perezoso) unaRutina = map (descansar 5 .) unaRutina
armarRutina (GuerreroZ _ _ _ _ Tramposo) _         = []

{-
Dependiendo el tipo de guerrero dado, en el caso de Yajirobe sí porque no lleva a cabo ningun ejercicio directamente.
Pero en el caso de Gohan o Vegetta no se podría ya que no hay forma de detener el armado de su rutina, no hay una
forma de cortar el ciclo: Vegetta realiza los ejercicios sin descansar, por ende todos los ejercicios que le mandes
los hará completamente mientras que Gohan llevará a cabo un descanso entre ejercicio infinitamente.

Para cortar el ciclo se debería de poder establecer un limite para que la lazy evaluation pudiera detenerse 
hasta donde se requiere ya que debería analizar el valor en ese momento.
-}

--------------------------------         Punto 5        --------------------------------

realizaUnaRutina :: GuerreroZ -> [Ejercicio] -> GuerreroZ
realizaUnaRutina unGuerrero unaRutina = foldr (($) . realizarUnEjercicio) unGuerrero . armarRutina unGuerrero $ unaRutina

--------------------------------         Punto 6        --------------------------------

descansar :: Float -> GuerreroZ -> GuerreroZ
descansar tiempoDescanso = alterarFatiga (subtract (descanso tiempoDescanso))

descanso :: Float -> Float
descanso tiempo = sum [1.0..tiempo]

--------------------------------         Punto 7        --------------------------------

cantidadOptimaMinutos :: GuerreroZ -> Float
cantidadOptimaMinutos unGuerrero
    | estaCansado unGuerrero = cantidadADescansar unGuerrero 0 -- descanso . fromIntegral . ceiling . cansancio $ unCansancio
    | otherwise              = 0

cantidadADescansar :: GuerreroZ -> Float -> Float
cantidadADescansar unGuerrero minutosADescansar
    | not . estaCansado . descansar minutosADescansar $ unGuerrero = minutosADescansar
    | otherwise                                                    = cantidadADescansar unGuerrero (minutosADescansar + 1)