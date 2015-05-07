module Imagen
  ( hSplit, vSplit
  , colorPromedio
  , subImagen
  , getDatos
  )
  where

import Data.List (genericTake, genericDrop)
import Graphics.Mosaico.Imagen (Color(Color, rojo, verde, azul), Imagen(Imagen, altura, anchura, datos))


subImagen
  :: Integer -> Integer
  -> Integer -> Integer
  -> Imagen -> Imagen

subImagen xInicial yInicial anchura' altura' imagen
     = Imagen anchura' altura' (subLista (datos imagen))
     where subLista = map f . genericTake altura' . genericDrop xInicial;
           f = genericTake anchura' . genericDrop yInicial

getDatos :: Imagen -> [[Color]]
getDatos (Imagen _ _ m) = m

hSplit :: Imagen -> (Imagen, Imagen)
hSplit = undefined

vSplit :: Imagen -> (Imagen, Imagen)
vSplit = undefined

colorPromedio :: Imagen -> Color
colorPromedio = undefined
