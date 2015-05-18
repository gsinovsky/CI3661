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
  = Imagen anchura' altura' (map (genericTake anchura') $ (map (genericDrop xInicial) $ genericTake altura' $ genericDrop yInicial $ (getDatos imagen)))

{-subImagen xInicial yInicial anchura' altura' imagen
     = Imagen anchura' altura' (subLista (datos imagen))
     where subLista = map f . genericTake altura' . genericDrop xInicial;
           f = genericTake anchura' . genericDrop yInicial
-}

getDatos :: Imagen -> [[Color]]
getDatos (Imagen _ _ m) = m

-- Por decision de impletancion si la altura es impar, la imagen superior sera mayor en una unidad con respecto
-- a la altura de la imagen inferior

hSplit :: Imagen -> (Imagen, Imagen)
hSplit imagen = (superior, inferior)
      where inferior = subImagen 0 0 anchura' ((altura imagen)- medio) imagen
            superior = subImagen 0 medioalt anchura' ((altura imagen) - medioalt) imagen
            anchura' = anchura imagen
            medio = if (altura imagen) `mod` 2 == 0 then ((altura imagen) `div` 2) else (((altura imagen) `div` 2)+1)
            medioalt = if ((altura imagen) `mod` 2 == 0) then medio else (medio - 1) 

{-hSplit :: Imagen -> (Imagen, Imagen)
hSplit imagen = (superior, inferior)
      where superior = subImagen 0 0 anchura' medio imagen
            inferior = subImagen 0 medio anchura' (altura'-medio) imagen
            anchura' = anchura imagen
            altura' = altura imagen
            medio = (altura'+1) `div` 2
-}


-- Por decision de impletancion si el ancho es impar, la imagen de la izquierda sera mayor en una unidad con respecto
-- a el ancho de la imagen derecha

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