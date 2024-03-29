% Tarea práctica de programación funcional
% Universidad Simón Bolívar — CI3661
% Abril–Julio 2015

Integrantes del grupo:

*   09-11207 <gsinovsky@gmail.com>
*   07-41249 <jotha41@gmail.com>



Introducción
============

Este documento es el enunciado de la tarea práctica de programación funcional para CI3661 (el Laboratorio de Lenguajes de Programación 1) en Abril–Julio de 2015.  La fuente de este documento está escrita en *Literate Haskell* con comentarios en *Markdown*, por lo cual puede usarse para producir la versión PDF de este documento[^pandoc], y también puede suministrarse directamente al compilador de *Haskell* GHC como código fuente.

[^pandoc]: Para esto se usa el programa *Pandoc* que, incidentalmente, está escrito en *Haskell*.

Puede cargar la fuente de este archivo en GHCi directamente con el comando

    ghci tarea-práctica-funcional

Todas las líneas de este archivo que no comiencen con `>` son comentarios en el código.  Las líneas que sí comiencen con `>` serán interpretadas por el compilador de *Haskell* como código fuente.  A lo largo de este enunciado, se proveerán definiciones incompletas en *Haskell* que usted debe completar.  En particular, usted deberá sustituir todas las ocurrencias de `undefined` en este archivo con sus soluciones para que el programa cumpla con la funcionalidad esperada.



Condiciones de entrega
----------------------

Esta tarea debe ser realizada por cada alumno de CI3661 en grupos de a lo sumo dos integrantes.  Debe entregar su solución en un archivo llamado `t2-XX-XXXXX_YY-YYYYY.lhs` (donde `XX-XXXXX` y `YY-YYYYY` deben ser sustituidos por los números de carné de los integrantes del grupo), o `t2-XX-XXXXX.lhs` para los casos excepcionales que realicen la tarea en forma individual, enviado adjunto a un correo electrónico titulado *[CI3661] Tarea 2* a las direcciones de *todos* los encargados del curso:

*   Manuel Gómez     <manuel.gomez.ch@gmail.com>
*   David Lilue      <dvdalilue@gmail.com>
*   Ricardo Monascal <rmonascal@gmail.com>
*   Wilmer Pereira   <wpereira@usb.ve>

Debe enviar su solución antes de la medianoche entre el domingo 2015-05-03 y el lunes 2015-05-04 en hora legal de Venezuela.



Declaraciones preliminares
--------------------------

En esta sección puede agregar todas las directivas necesarias para importar símbolos de módulos adicionales a `Prelude` (que se importa implícitamente) y para dar opciones al compilador.

> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE TypeSynonymInstances #-}
>
> import Control.Applicative (pure)
> import Control.DeepSeq     (NFData, ($!!))
> import Control.Monad       (void)
> import Data.Map            (Map, empty, singleton, toList, assocs)
> import GHC.Generics        (Generic)
> import System.Environment  (getArgs, getProgName)
> import System.IO           (hPutStrLn, stderr)



Expresiones aritméticas
=======================

Considere la siguiente declaración de un tipo algebráico para representar expresiones aritméticas con números de punto flotante:

> data Expresión
>   = Suma           Expresión Expresión
>   | Resta          Expresión Expresión
>   | Multiplicación Expresión Expresión
>   | División       Expresión Expresión
>   | Negativo       Expresión
>   | Literal        Integer
>   deriving (Eq, Read, Show)

---

**Ejercicio 1** (0.2 puntos cada una; 0.6 puntos en total): Complete las siguientes definiciones para que `t1`, `t2` y `t3` representen a las respectivas expresiones aritméticas:

----- --- ---
`t1`   =  `42`
`t2`   =  `27 + t1`
`t3`   =  `(t2 * (t2 * 1)) + (- ((t1 + 0) / 3))`
----- --- ---

> t1, t2, t3 :: Expresión
> t1 = Literal 42
> t2 = Suma t1 (Literal 27)
> t3 = Suma (Multiplicación t2 (Multiplicación t2 (Literal 1))) (Negativo (División (Suma t1 (Literal 0)) (Literal 3)))

---



Catamorfismos
-------------

Los valores de cada tipo algebraico en *Haskell* son datos que representan la estructura abstracta de una operación, sin especificar cuál es la operación particular que debe hacerse.  Un valor de un tipo algebraico puede analizarse para convertirlo en una operación particular: por ejemplo, el valor `t2` representa la estructura abstracta de la operación «la suma de 42 y 27», sin decir qué significa «la suma».

Un valor de esta forma puede convertirse en una operación particular de varias maneras.  Si bien la interpretación más obvia de esa operación abstracta corresponde con la suma aritmética de los números 42 y 27, que produce un número, también podrían realizarse *otras* operaciones siguiendo la misma estructura.  Por ejemplo, puede buscarse cuál es el máximo operando de todas las operaciones, en cuyo caso la operación especificada como «suma» sería tomar el máximo entre dos números, y en el caso de `t2` produciría como resultado el número 42.

En el contexto de *Haskell*, un **catamorfismo** es cualquier transformación de un tipo algebraico a una operación en otro tipo que se haga de esta manera: según la estructura del valor del tipo algebraico.

---

**Ejercicio 2** (0.5 puntos): Complete la siguiente definición que calcule el resultado de evaluar una expresión aritmética.

> evaluar :: Expresión -> Double
> evaluar
>   = \ case
>     Suma           e1 e2 -> (evaluar e1) + (evaluar e2)
>     Resta          e1 e2 -> (evaluar e1) - (evaluar e2)
>     Multiplicación e1 e2 -> (evaluar e1) * (evaluar e2)
>     División       e1 e2 -> (evaluar e1) / (evaluar e2)
>     Negativo       e     -> negate (evaluar e)
>     Literal        n     -> fromIntegral n

En particular,

    evaluar t1 == 42.0
    evaluar t2 == 69.0
    evaluar t3 == 4747.0

---

**Ejercicio 3** (0.25 puntos): Complete la siguiente definición que calcule la cantidad de operaciones aritméticas especificadas en una expresión aritmética.  Se considera que la expresión correspondiente a un simple literal especifica cero operaciones.

> operaciones :: Expresión -> Integer
> operaciones
>   = \ case
>     Suma           e1 e2 -> 1 + (operaciones e1) + (operaciones e2)
>     Resta          e1 e2 -> 1 + (operaciones e1) + (operaciones e2)
>     Multiplicación e1 e2 -> 1 + (operaciones e1) + (operaciones e2)
>     División       e1 e2 -> 1 + (operaciones e1) + (operaciones e2)
>     Negativo       e     -> 1 + (operaciones e)
>     Literal        n     -> 0


En particular,

    operaciones t1 == 0
    operaciones t2 == 1
    operaciones t3 == 8

---

**Ejercicio 4** (0.25 puntos): Complete la siguiente definición que calcule la suma de todos los literales presentes en una expresión aritmética.

> sumaLiterales :: Expresión -> Integer
> sumaLiterales
>   = \ case
>     Suma           e1 e2 -> (sumaLiterales e1) + (sumaLiterales e2)
>     Resta          e1 e2 -> (sumaLiterales e1) + (sumaLiterales e2)
>     Multiplicación e1 e2 -> (sumaLiterales e1) + (sumaLiterales e2)
>     División       e1 e2 -> (sumaLiterales e1) + (sumaLiterales e2)
>     Negativo       e     -> sumaLiterales e
>     Literal        n     -> n

En particular,

    sumaLiterales t1 == 42
    sumaLiterales t2 == 69
    sumaLiterales t3 == 184

---

**Ejercicio 5** (0.25 puntos): Complete la siguiente definición que calcule la lista de todos los literales presentes en una expresión aritmética.

> literales :: Expresión -> [Integer]
> literales
>   = \ case
>     Suma           e1 e2 -> (literales e1) ++ (literales e2)
>     Resta          e1 e2 -> (literales e1) ++ (literales e2)
>     Multiplicación e1 e2 -> (literales e1) ++ (literales e2)
>     División       e1 e2 -> (literales e1) ++ (literales e2)
>     Negativo       e     -> literales e
>     Literal        n     -> [n]

En particular,

    literales t1 == [42]
    literales t2 == [27, 42]
    literales t3 == [27, 42, 27, 42, 1, 42, 0, 3]

---

**Ejercicio 6** (0.25 puntos): Complete la siguiente definición que calcule la altura de una expresión aritmética.  Se considera que un literal es una expresión aritmética de altura cero, y que todas las demás operaciones agregan uno a la altura.


> altura :: Expresión -> Integer
> altura
>   = \ case
>     Suma           e1 e2 -> 1 + (altura e1) `max`(altura e2)
>     Resta          e1 e2 -> 1 + (altura e1) `max` (altura e2)
>     Multiplicación e1 e2 -> 1 + (altura e1) `max` (altura e2)
>     División       e1 e2 -> 1 + (altura e1) `max` (altura e2)
>     Negativo       e     -> 1 + (altura e)
>     Literal        n     -> 0

En particular,

    altura t1 == 0
    altura t2 == 1
    altura t3 == 4

---



Catamorfismo generalizado
-------------------------

Los catamorfismos de las preguntas anteriores deben seguir un patrón común: cada constructor del tipo `Expresión` se hace corresponder con una operación en el tipo del resultado del catamorfismo.  Esas operaciones se realizan con los valores almacenados en cada constructor, y en el caso de las ocurrencias anidadas de valores del tipo `Expresión`, éstas se transforman a valores del tipo del resultado con una invocación recursiva al catamorfismo.

En efecto, todo catamorfismo se construye de la misma forma para un tipo algebraico dado — solo es necesario especificar de qué manera se combinan a un valor del tipo resultante los datos obtenidos de cada constructor.

---

**Ejercicio 7** (0.5 puntos): Complete la siguiente definición para el catamorfismo generalizado del tipo `Expresión`.

> cataExpresión
>   :: (a -> a -> a)
>   -> (a -> a -> a)
>   -> (a -> a -> a)
>   -> (a -> a -> a)
>   -> (a -> a)
>   -> (Integer -> a)
>   -> Expresión -> a
>
> cataExpresión
>   suma
>   resta
>   multiplicación
>   división
>   negativo
>   literal
>       = \ case 
>         Suma e1 e2 -> suma (f e1) (f e2)
>         Resta e1 e2 -> resta (f e1) (f e2)
>         Multiplicación e1 e2 -> multiplicación (f e1) (f e2)
>         División e1 e2 -> división (f e1) (f e2)
>         Negativo e1 -> negativo (f e1)
>         Literal n  -> literal n
>   where f = cataExpresión suma resta multiplicación división negativo literal



**Ejercicio 8** (0.2 puntos cada una; 1 punto en total): Complete las siguientes definiciones para los catamorfismos que definió en las preguntas anteriores, esta vez en términos de `cataExpresión`.

> evaluar' :: Expresión -> Double
> evaluar' = cataExpresión (+) (-) (*) (/) negate fromIntegral
>
> operaciones' :: Expresión -> Integer
> operaciones' = cataExpresión suma resta multiplicacion division negativo literal
>     where suma a b = 1 + a + b
>           resta a b = 1 + a + b
>           multiplicacion a b = 1 + a + b
>           division a b = 1 + a + b
>           negativo a = 1 + a
>           literal n = 0
>
> sumaLiterales' :: Expresión -> Integer
> sumaLiterales' = cataExpresión (+) (+) (+) (+) id id
>
> literales' :: Expresión -> [Integer]
> literales' = cataExpresión suma resta multiplicacion division negativo literal
>     where suma a b = a ++ b
>           resta a b = a ++ b
>           multiplicacion a b = a ++ b
>           division a b = a ++ b
>           negativo a = a
>           literal n = [n]
>
> altura' :: Expresión -> Integer
> altura' = cataExpresión suma resta multiplicacion division negativo literal
>     where suma a b = 1 + a `max` b
>           resta a b = 1 + a `max` b
>           multiplicacion a b = 1 + a `max` b
>           division a b = 1 + a `max` b 
>           negativo a = 1 + a
>           literal n = 0


---



Lenguajes de marcado
====================

Los lenguajes de marcado descendientes de SGML se utilizan para especificar documentos jerárquicos, donde el texto del documento se incluye en *elementos* que pueden anidarse y se clasifican según el *nombre de etiqueta* de cada uno.  Además, los elementos pueden asociarse con un diccionario de *atributos* textuales identificados por un nombre de atributo.

Puede representarse una versión simplificada de esta idea con los siguientes tipos de datos algebraicos de *Haskell*:

> type Atributos
>   = Map String String
>
> newtype Documento
>   = Documento Elemento
>   deriving Show
>
> data Elemento
>   = Elemento String Atributos [Elemento]
>   | Texto String
>   deriving Show

Los documentos completos están formados por un elemento como raíz.  Los elementos pueden tener un nombre de etiqueta, un diccionario de atributos (usando el tipo `Map` definido en el módulo `Data.Map`), y una lista de elementos anidados — salvo en el caso de los elementos que sencillamente contienen texto.

---



Combinadores
------------

**Ejercicio 9** (0.15 puntos cada una; 0.6 puntos en total): Complete las siguientes definiciones para combinadores que produzcan representaciones de los elementos de XHTML `html`, `head`, `body` y `div` a partir de una lista de elementos anidados dentro de ellos.  Los elementos resultantes de aplicar estos combinadores deben tener diccionarios de atributos vacíos, salvo en el caso del elemento `html`, que debe incluir exactamente un atributo que debe llamarse `xmlns` y asociarse al valor `http://www.w3.org/1999/xhtml`.[^sufijos]

[^sufijos]: Se utiliza el sufijo `E` para evitar conflictos con los nombres `head` y `div` importados implícitamente desde el módulo `Prelude` de *Haskell*.

> htmlE, headE, bodyE, divE :: [Elemento] -> Elemento
> htmlE  = Elemento "html" (Data.Map.singleton "xmlns" "http://www.w3.org/1999/xhtml")
> headE  = Elemento "head" empty 
> bodyE  = Elemento "body" empty
> divE   = Elemento "div" empty 

---

**Ejercicio 10** (0.15 puntos cada una; 0.6 puntos en total): Complete las siguientes definiciones para combinadores que produzcan representaciones de los elementos de XHTML `title`, `style`, `h1` y `p` a partir de un `String` con el texto que debe incluirse dentro de ellos.  Los elementos resultantes de aplicar estos combinadores deben tener diccionarios de atributos vacíos, salvo el elemento `style` que debe tener el atributo `type` asociado al texto `text/css`.

> styleE, titleE, h1E,pE :: String -> Elemento
> styleE t = Elemento "style" (Data.Map.singleton "type" "text/css") [Texto t]
> titleE t = Elemento "title" Data.Map.empty [Texto t]
> h1E t    = Elemento "h1" Data.Map.empty [Texto t]
> pE t     = Elemento "p" Data.Map.empty [Texto t]

---

**Ejercicio 11** (0.2 puntos): Complete la siguiente definición para un combinador que produzca una representación del elemento de XHTML `p` a partir de un valor de cualquier tipo `a` que pertenezca a la clase de tipos `Show`; el elemento `p` resultante de aplicar este combinador debe contener únicamente un nodo de texto cuyo `String` sea el resultante de aplicar la función `show` al valor pasado como parámetro, y debe tener su diccionario de atributos vacío.
 
> showP :: Show a => a -> Elemento
> showP a
>   | (fst var) == "Texto" = pE (takeWhile (/= '"') $ drop 1 $ dropWhile (/= '"') (snd var)) 
>   | (fst var) == "Elemento" = pE (takeWhile (/= '"') $ drop 1 $ dropWhile (/= '"') (snd var))
>   | otherwise = pE (show a)
>   where 
>       var = splitAt 5 (show a)
>       var2 = splitAt 8 (show a)

---



Generación de XHTML
-------------------

Considere la siguiente clase de tipos:

> class RenderXHTML a where
>   render :: a -> String

Se desea usar el método `render` para generar texto XHTML a partir de un valor de cualquier tipo que sea instancia de esta clase de tipos.[^noshow]  Para convertir un `Documento` a su código XHTML, se declara que `Documento` es una instancia de `RenderXHTML`:

[^noshow]: Note que esta clase es estructuralmente idéntica a la clase `Show` de *Haskell*.  Sin embargo, recuerde que el propósito de la clase `Show` es construir representaciones textuales de valores de *Haskell* para asistir al programador a estudiar un tipo de datos y visualizar sus valores — la clase `RenderXHTML`, en cambio, existe para generar código XHTML a partir de algunos tipos de datos que puedan convertirse de esa manera.

> instance RenderXHTML Documento where
>   render (Documento raíz)
>     = encabezado ++ render raíz
>     where
>       encabezado
>         = unlines
>           [ "<?xml version='1.0' encoding='UTF-8'?>"
>           , "<!DOCTYPE html"
>           , "     PUBLIC '-//W3C//DTD XHTML 1.0 Strict//EN'"
>           , "     'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>"
>           ]

---

**Ejercicio 12** (1.25 puntos): Escriba una instancia de la clase `RenderXHTML` para el tipo `Atributos`.  Puede suponer que las claves de los diccionarios de atributos únicamente contienen nombres de atributos válidos para XHTML, y que los valores de atributos no contienen entidades ilegales en XHTML ni comillas — es decir, no es necesario que se preocupe por escapar el texto obtenido del diccionario de atributos.

El texto generado debe corresponder a la lista de atributos que ocurre dentro de una etiqueta XHTML.  Por ejemplo, para el diccionario de atributos

Nombre de atributo  Valor
------------------- ------
foo                 bar baz
quux                meh
wtf                 wow://such.example.com/amaze/

debe generar el texto

    foo='bar baz' quux='meh' wtf='wow://such.example.com/amaze/'

El orden en que genere las especificaciones de atributos es irrelevante.

-- Funcion auxiliar para hacer el fold a la lista generada a partir del diccionario

> aux :: [(String,String)] -> String
> aux = foldr(\x acc -> (" " ++ (fst x) ++ "=" ++ "'" ++(snd x)++"'")++acc ) "" 

----------


> instance RenderXHTML Atributos where
>   render x  = if x== empty then "" else aux $ Data.Map.assocs x



**Ejercicio 13** (1.25 puntos): Escriba una instancia de la clase `RenderXHTML` para el tipo `Elemento`.  Puede suponer que los nombres de etiquetas de los elementos siempre son válidos para XHTML, y que los nodos de texto no contienen entidades ilegales ni caracteres reservados por XHTML — es decir, no es necesario que se preocupe por escapar el texto obtenido del elemento.

El texto generado debe corresponder a una etiqueta XHTML para el elemento dado.  Por ejemplo, para un elemento con el nombre de etiqueta `welp`, y un hijo que sea un nodo textual con el texto `trololololo`, debe generar el texto

    <welp foo='bar baz' quux='meh' wtf='wow://such.example.com/amaze/'>trololololo</welp>

o cualquier texto con el mismo significado en XHTML — el espacio en blanco, por ejemplo, es irrelevante.

--Funciones auxiliares 

> obtenerEtiqueta:: Elemento -> String
> obtenerEtiqueta (Elemento x _ _) = x
> obtenerEtiqueta (Texto _) = ""

> obtenerAtrib:: Elemento -> Atributos
> obtenerAtrib (Elemento _ x _) = x
> obtenerAtrib (Texto _) = empty

> obtenerElemL:: Elemento -> [Elemento]
> obtenerElemL (Elemento _ _ x) = x
> obtenerElemL (Texto _) = []

> obtenerTexto:: Elemento -> String
> obtenerTexto (Elemento _ _ _) = ""
> obtenerTexto (Texto x) = x 

-- Es necesario poder hacer render a la lista de elementos, por
-- eso se emplea esta funcion donde se instancia render a [Elemento]

> instance RenderXHTML [Elemento] where
>   render [] = ""
>   render [x] = (obtenerTexto x) ++ 
>       if  (obtenerEtiqueta x) /= "" then "<" ++ (obtenerEtiqueta x) 
>           ++ (render $ obtenerAtrib x) ++ ">" ++ (render $ obtenerElemL x) ++ "</" ++ (obtenerEtiqueta x) ++ ">"
>           else ""
>   render (x:xs) = (obtenerTexto x) ++ 
>       if  (obtenerEtiqueta x) /= "" then "<" ++ (obtenerEtiqueta x)
>           ++ (render $ obtenerAtrib x) ++ ">" ++ (render $ obtenerElemL x) ++ "</" ++ (obtenerEtiqueta x) ++ ">" ++ render xs
>           else "" ++ render xs

> instance RenderXHTML Elemento where
>   render x = render [x] 

---



XHTML para expresiones
----------------------

**Ejercicio 14** (1.25 puntos): Complete la siguiente definición que convierta un valor dado del tipo `Expresión` en un valor del tipo `Elemento` que represente a la estructura de la expresión aritmética con un árbol de elementos XHTML.

Los literales numéricos deben representarse con elementos `p` que contengan un nodo de texto con una representación textual del número.

Las operaciones aritméticas deben representarse con un elemento `div` que contenga a los operandos transformados en elementos hijos, y que indique la operación con un elemento `p` que contenga un nodo de texto con el símbolo correspondiente a la operación — en el caso de operaciones binarias, como la suma, ubique el símbolo entre los elementos hijos correspondientes a los operandos, y en el caso del negativo, ubique el símbolo `-` antes que el elemento hijo.

Escriba su definición en términos de `cataExpresión` y utilice los combinadores para elementos de XHTML que definió previamente.


> expresiónXHTML :: Expresión -> Elemento
> expresiónXHTML = cataExpresión suma resta multiplicacion division negativo literal
>     where suma a b = divE  [a, pE "+", b]
>           resta a b = divE [a, pE "-", b]
>           multiplicacion a b = divE  [a, pE "*", b]
>           division a b = divE  [a, pE "/", b]
>           negativo a = divE [pE "-", a]
>           literal n = showP n


Por ejemplo, el resultado de `expresiónXHTML t2` debería ser igual al de

    Elemento "div" empty
      [ Elemento "p" empty [Texto "27"]
      , Elemento "p" empty [Texto "+" ]
      , Elemento "p" empty [Texto "42"]
      ]

donde `empty` viene del módulo `Data.Map`.

---

**Ejercicio 15** (1.25 puntos): Complete la siguiente definición que convierta un valor dado del tipo `Expresión` en un valor del tipo `Documento` que muestre información sobre la expresión aritmética dada en un documento XHTML.  Debe usar los combinadores definidos en ejercicios previos para implantar esta función.

Se espera que el documento generado sea lo más parecido posible al mostrado en la dirección <https://ldc.usb.ve/~05-38235/cursos/CI3661/2015AJ/expresión.xhtml>[^t3xhtml].  En particular, debe mostrar:

*   la representación textual de la expresión,
*   la estructura de la expresión convertida en elementos de XHTML usando la función `expresiónXHTML`,
*   el resultado de la evaluación numérica de la expresión,
*   la altura de la expresión,
*   el número de operaciones de la expresión, y
*   la lista de todos los literales numéricos que ocurren en la expresión.

Antes de cada una de esas secciones, incluya un elemento `h1` con el nombre de la sección.

[^t3xhtml]: Ese ejemplo fue generado a partir de la expresión `t3`.

> expresiónDocumento :: Expresión -> Documento
> expresiónDocumento e = Documento (
>     htmlE [headE [titleE "Expresión", styleE estilo], 
>          bodyE [
>                  h1E "Expresión original", showP e, 
>                  h1E "Estructura", expresiónXHTML e, 
>                  h1E "Valor", showP (evaluar e), 
>                  h1E "Altura", showP (altura e),
>                  h1E "Número de operaciones", showP (operaciones e),
>                  h1E "Literales", showP (literales' e)
>                ]
>            ])


La siguiente definición contiene el texto necesario a incluir en el elemento `style` del documento a generar.  El elemento `style` con este contenido debe ser incluido en el elemento `head` del documento generado.

> estilo :: String
> estilo
>   = unlines
>     [ "div, p {"
>     , "  border: 1px solid black;"
>     , "  float: left;"
>     , "  margin: 1em;"
>     , "}"
>     , "h1 {"
>     , "  clear: both;"
>     , "}"
>     ]



---



Programa principal
==================

Se define un programa principal para poder compilar y ejecutar este archivo:

> deriving instance Generic Expresión
> instance NFData Expresión
>
> main :: IO ()
> main = do
>   args <- getArgs
>   case args of
>     (nombreArchivo : expresiónTexto : _) -> do
>       expresión <- pure $!! read expresiónTexto
>       writeFile nombreArchivo . render $ expresiónDocumento expresión
>
>     _ -> do
>       progName <- getProgName
>       hPutStrLn stderr $ "Uso: " ++ progName ++ " ARCHIVO.xhtml EXPRESIÓN"

Puede compilar la fuente de este archivo con el comando

    ghc tarea-práctica-funcional.lhs -o expresión

y probar su solución con, por ejemplo, el comando

    ./expresión expresión.xhtml 'Suma (Literal 42) (Literal 27)'
