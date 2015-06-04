module Imagen
  ( hSplit, vSplit
  , colorPromedio
  , subImagen
  , hSplit
  )
  where

import Data.List (genericTake, genericDrop)
import Graphics.Mosaico.Imagen (Color(Color, rojo, verde, azul), Imagen(Imagen, altura, anchura, datos))

getDatos :: Imagen -> [[Color]]
getDatos (Imagen _ _ m) = m


subImagen
  :: Integer -> Integer
  -> Integer -> Integer
  -> Imagen -> Imagen
subImagen xInicial yInicial anchura' altura' imagen
  = Imagen anchura' altura' (map (genericTake anchura') $ (map (genericDrop xInicial) $ genericTake altura' $ genericDrop yInicial $ (getDatos imagen)))


hSplit :: Imagen -> (Imagen, Imagen)
hSplit imagen = (superior, inferior)
      where superior = subImagen 0 0 anchura' medio imagen
            inferior = subImagen 0 medio anchura' (altura'-medio) imagen
            anchura' = anchura imagen
            altura' = altura imagen
            medio = (altura'+1) `div` 2


vSplit :: Imagen -> (Imagen, Imagen)
vSplit imagen = (izquierda, derecha)
      where izquierda = subImagen 0 0 medio altura' imagen
            derecha = subImagen medio 0 (anchura'-medio) altura' imagen
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