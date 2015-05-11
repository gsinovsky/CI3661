module Imagen
  ( hSplit, vSplit
  , colorPromedio
  , subImagen
  , getDatos
  , hSplit
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
hSplit imagen = (superior, inferior)
      where superior = subImagen 0 0 anchura' medio imagen
            inferior = subImagen 0 medio anchura' (altura'-medio) imagen
            anchura' = anchura imagen
            altura' = altura imagen
            medio = (altura'+1) `div` 2


vSplit :: Imagen -> (Imagen, Imagen)
vSplit imagen = (superior, inferior)
      where superior = subImagen 0 0 medio altura' imagen
            inferior = subImagen medio 0 (anchura'-medio) altura' imagen
            anchura' = anchura imagen
            altura' = altura imagen
            medio = (anchura'+1) `div` 2



colorPromedio :: Imagen -> Color
colorPromedio i = promedio 0 0 0 0 . concat . datos  $ i
     where promedio :: Integer -> Integer -> Integer -> Integer -> [Color] -> Color
           promedio r v a n [] = Color { rojo  = round ((fromInteger r) / (fromInteger n))
                                       , verde = round ((fromInteger v) / (fromInteger n))
                                       , azul  = round ((fromInteger a) / (fromInteger n))
                                       }
           promedio r v a n (Color r' v' a' : cs) = promedio (r + (fromIntegral r')) (v + (fromIntegral v')) (a+(fromIntegral a')) (n+1) cs