-- La función "flip" recibe una funcion y dos parametros para operar la misma, invirtiendo los mismos
-- para luego aplicar la función colocada.
-- Ej: flip (/) 2 1; uno esperaria que se hiciera 2 / 1 pero lo que hace es invertir el uso de los
-- parametros haciendo que se ejecute: 1 / 2.
-- flip f a b = f b a

----------------------------- DECLARANDO FUNCIONES CONOCIDAS -----------------------------

-- Al momento de usarlo en la consola, poner Main.funcion (siendo funcion el nombre de la declarada aca)
-- para no confundir al programa con la función original.

max :: Ord a => a -> a -> a
max a b | a >= b = a
        | a < b = b