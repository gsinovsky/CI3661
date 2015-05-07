module Main (main) where

import Graphics.Mosaico.Diagrama (Diagrama((:-:), (:|:), Hoja), Paso(Primero, Segundo))
import Graphics.Mosaico.Imagen   (Imagen(Imagen, altura, anchura, datos), leerImagen)
import Graphics.Mosaico.Ventana  (Ventana, cerrar, crearVentana, leerTecla, mostrar)

import Diagramas (Orientación(Horizontal, Vertical), caminar, dividir, rectánguloImagen, sustituir)
import Imagen (subImagen)

ciclo :: Ventana -> Diagrama -> [Paso] -> IO ()
ciclo = undefined

main :: IO ()
main = undefined
