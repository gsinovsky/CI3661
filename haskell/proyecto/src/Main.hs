module Main (main) where

import Graphics.Mosaico.Diagrama (Diagrama((:-:), (:|:), Hoja), Paso(Primero, Segundo))
import Graphics.Mosaico.Imagen   (Imagen(Imagen, altura, anchura, datos), Color(Color, rojo, verde, azul), leerImagen)
import Graphics.Mosaico.Ventana  (Ventana, cerrar, crearVentana, leerTecla, mostrar)
import System.Environment (getArgs)
import Safe (initSafe)

import Diagramas (Orientación(Horizontal, Vertical), caminar, dividir, rectánguloImagen, sustituir)


ciclo :: Ventana -> Diagrama -> [Paso] -> IO ()
ciclo ventana diagrama pasos = do
	mostrar ventana pasos diagrama
	tecla <- leerTecla ventana
	case tecla of
		Nothing -> return ()
		Just "q" -> cerrar ventana
		Just tecla' -> 
		     let (diagrama', pasos') = auxiliar tecla' 
		      in ciclo ventana diagrama' pasos'
    where
    	auxiliar tecla'
    	 = case tecla' of
    	 	"BackSpace" -> (diagrama, initSafe pasos)
    	 	"Up" -> case caminar pasos diagrama of
    	 		Nothing -> pararse
    	 		Just (Hoja rectángulo) ->
    	 		     case dividir Horizontal rectángulo of 
    	 		     	Nothing -> pararse
    	 		     	Just d' -> case sustituir d' pasos diagrama of
    	 		     		Nothing -> pararse
    	 		     		Just diagrama' -> (diagrama', pasos++[Primero])
 		     	Just (_ :-: _) -> (diagrama, pasos++[Primero])
 		     	Just (_ :|: _) -> pararse
    	 	"Down" -> case caminar pasos diagrama of
    	 		Nothing -> pararse
    	 		Just (Hoja rectángulo) ->
    	 		     case dividir Horizontal rectángulo of 
    	 		     	Nothing -> pararse
    	 		     	Just d' -> case sustituir d' pasos diagrama of
    	 		     		Nothing -> pararse
    	 		     		Just diagrama' -> (diagrama', pasos++[Segundo])
 		     	Just (_ :-: _) -> (diagrama, pasos++[Segundo])
 		     	Just (_ :|: _) -> pararse
    	 	"Left" -> case caminar pasos diagrama of
    	 		Nothing -> pararse
    	 		Just (Hoja rectángulo) ->
    	 		     case dividir Vertical rectángulo of 
    	 		     	Nothing -> pararse
    	 		     	Just d' -> case sustituir d' pasos diagrama of
    	 		     		Nothing -> pararse
    	 		     		Just diagrama' -> (diagrama', pasos++[Primero])
 		     	Just (_ :-: _) -> pararse
 		     	Just (_ :|: _) -> (diagrama, pasos++[Primero])
    	 	"Right" -> case caminar pasos diagrama of
    	 		Nothing -> pararse
    	 		Just (Hoja rectángulo) ->
    	 		     case dividir Vertical rectángulo of 
    	 		     	Nothing -> pararse
    	 		     	Just d' -> case sustituir d' pasos diagrama of
    	 		     		Nothing -> pararse
    	 		     		Just diagrama' -> (diagrama', pasos++[Segundo])
 		     	Just (_ :-: _) -> pararse
 		     	Just (_ :|: _) -> (diagrama, pasos++[Segundo]) 	 		     	 	 		     	 		     	
    	 	_ -> pararse

    	 	where 
    	 		pararse = (diagrama, pasos)


main :: IO ()
main = do
	args <- getArgs
	leer <- leerImagen (head args)
	case leer of 
		Right imagen -> do
			ventana <- crearVentana (anchura imagen) (altura imagen) 
			ciclo ventana (Hoja (rectánguloImagen imagen)) []
		Left e -> putStrLn e
