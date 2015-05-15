module
  Diagramas
  ( rectánguloImagen
  , Orientación(Horizontal, Vertical)
  , dividir
  , caminar
  , sustituir
  )
  where

import Graphics.Mosaico.Diagrama (Diagrama((:-:), (:|:), Hoja), Paso(Primero, Segundo), Rectángulo(Rectángulo, color, imagen))
import Graphics.Mosaico.Imagen   (Imagen(Imagen, altura, anchura, datos))

import Imagen (colorPromedio, hSplit, vSplit)



rectánguloImagen :: Imagen -> Rectángulo
rectánguloImagen  i = Rectángulo (colorPromedio i) i 

data Orientación
  = Horizontal
  | Vertical
  deriving Show

dividir :: Orientación -> Rectángulo -> Maybe Diagrama
dividir Horizontal (Rectángulo _ imagen') = if anchura imagen' >= 2 
  then Just $ Hoja (rectánguloImagen superior) :-: Hoja (rectánguloImagen inferior)
  else Nothing
  where (superior, inferior) = hSplit imagen'
dividir Vertical (Rectángulo _ imagen') = if altura imagen' >= 2 
  then Just $ Hoja (rectánguloImagen izquierda) :|: Hoja (rectánguloImagen derecha)
  else Nothing
  where (izquierda, derecha) = vSplit imagen'

caminar :: [Paso] -> Diagrama -> Maybe Diagrama
caminar [] d = Just d
caminar (_:_) (Hoja _) = Nothing
caminar (Primero:xs) (p :-: _) = caminar xs p
caminar (Primero:xs) (p :|: _) = caminar xs p
caminar (Segundo:xs) (_ :-: s) = caminar xs s
caminar (Segundo:xs) (_ :|: s) = caminar xs s



sustituir :: Diagrama -> [Paso] -> Diagrama -> Maybe Diagrama
sustituir d' [] _ = Just d'
sustituir _ _ (Hoja _) = Nothing
sustituir d' (Primero:xs) (p :-: s) = 
  case sustituir d' xs p of
    Just p' -> Just $ p' :-: s
    Nothing -> Nothing
sustituir d' (Primero:xs) (p :|: s) =
    case sustituir d' xs p of
    Just p' -> Just $ p' :|: s
    Nothing -> Nothing
sustituir d' (Segundo:xs) (p :-: s) =
    case sustituir d' xs s of
    Just s' -> Just $ p :-: s'
    Nothing -> Nothing
sustituir d' (Segundo:xs) (p :|: s) =
    case sustituir d' xs s of
    Just s' -> Just $ p :|: s'
    Nothing -> Nothing

