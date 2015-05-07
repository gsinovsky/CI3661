module Imagen
  ( hSplit, vSplit
  , colorPromedio
  , subImagen
  )
  where

import Graphics.Mosaico.Imagen (Color(Color, rojo, verde, azul), Imagen(Imagen, altura, anchura, datos))



subImagen
  :: Integer -> Integer
  -> Integer -> Integer
  -> Imagen -> Imagen

subImagen xInicial yInicial anchura' altura' imagen
     = Imagen anchura' altura' (subLista (datos imagen))
     where subLista = take (fromIntegral altura') . drop (fromIntegral xInicial) 


hSplit :: Imagen -> (Imagen, Imagen)
hSplit = undefined

vSplit :: Imagen -> (Imagen, Imagen)
vSplit = undefined

colorPromedio :: Imagen -> Color
colorPromedio = undefined
