module Main (main) where

import Graphics.Mosaico.Diagrama (Diagrama((:-:), (:|:), Hoja), Paso(Primero, Segundo))
import Graphics.Mosaico.Imagen   (Imagen(Imagen, altura, anchura, datos), leerImagen)
import Graphics.Mosaico.Ventana  (Ventana, cerrar, crearVentana, leerTecla, mostrar)
import System.Environment (getArgs)

import Diagramas (Orientación(Horizontal, Vertical), caminar, dividir, rectánguloImagen, sustituir)
import Imagen (subImagen, getDatos, hSplit)
import Graphics.Mosaico.Imagen (Color(Color, rojo, verde, azul), Imagen(Imagen, altura, anchura, datos))

ciclo :: Ventana -> Diagrama -> [Paso] -> IO ()
ciclo ventana diagrama pasos = do
	mostrar ventana pasos diagrama
	tecla <- leerTecla ventana
	case tecla of
		Just "q" -> cerrar ventana
		Nothing -> return ()
		Just tecla' -> 
		     let (diagrama', pasos') = auxiliar tecla' 
		      in ciclo ventana diagrama' pasos'
    where
    	auxiliar tecla'
    	 = case tecla' of
    	 	"BackSpace" -> (diagrama, init pasos)
    	 	"Up" -> case caminar pasos diagrama of
    	 		Just (Hoja rectángulo) ->
    	 		     case dividir Horizontal rectángulo of 
    	 		     	Nothing -> quedarse
    	 		     	Just d' -> case sustituir d' pasos diagrama of
    	 		     		Nothing -> quedarse
    	 		     		Just diagrama' -> (diagrama', pasos++[Primero])
 		     	Just (_ :-: _) -> (diagrama, pasos++[Primero])
 		     	Just (_ :|: _) -> quedarse
    	 	"Down" -> case caminar pasos diagrama of
    	 		Just (Hoja rectángulo) ->
    	 		     case dividir Horizontal rectángulo of 
    	 		     	Nothing -> quedarse
    	 		     	Just d' -> case sustituir d' pasos diagrama of
    	 		     		Nothing -> quedarse
    	 		     		Just diagrama' -> (diagrama', pasos++[Segundo])
 		     	Just (_ :-: _) -> (diagrama, pasos++[Segundo])
 		     	Just (_ :|: _) -> quedarse 		     	
    	 	_ -> quedarse

    	 	where 
    	 		quedarse = (diagrama, pasos)


main :: IO ()
main = do
	args <- getArgs
	leer <- leerImagen (head args)
	case leer of 
		Right imagen -> do
			ventana <- crearVentana (anchura imagen) (altura imagen) 
			ciclo ventana (Hoja (rectánguloImagen imagen)) []
		Left e -> putStrLn e
