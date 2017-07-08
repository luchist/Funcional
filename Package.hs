
-- Paquetes contenedores de datos ----

-- Recordar que las clases de Haskell no tienen NADA que ver con clases de objetos
-- sino que recuerdan mas a una interfaz, donde se definen el tipo de las funciones
-- Para que algo sea instancia de la clase debe implementar todas las funciones definidas
-- en la misma, respetando los tipos.

class Package p where
    handle::(a->b) -> p a -> p b
--            \
--         funcion que quiero aplicar "adentro" del paquete

--  p a => paquete de tipo a
--  p b => paquete de tipo b (osea que puede cambiar el tipo del contenido, o no, PERO sigue siendo un paquete)

-- ¿Que hace handle?
-- Genera un nuevo paquete de igual estructura p pero que contiene datos de tipo b.
-- En cierta forma, es como si aplicara la funcion adentro de los paquetes.

-- -----------------------------------
-- -----------------------------------


-- Implementacion de Package para el tipo Maybe --

-- ¿Como se implementan instancias de Clases para distintos tipos?
-- Usamos la keyword instance para decir que Maybe es instacia de Package
-- Despues necesitamos implementar handle para el tipo Maybe

instance Package Maybe where
    handle f (Just x) = Just (f x)              --  2 ) Si era Just, aplicamos f "adentro" del maybe, y volvemos a "poner la tapa"
    handle f Nothing  = Nothing                 --  3 ) si era Nothing simplemente devolvemos Nothing
--              |
--        1) abrimos el tipo, osea usamos pattern matching
--           para saber que hacer en cada caso

-- Osea que : tanto el Just como el Nothing se "propagan"


-- Implementacion de Package para el tipo Listas --

instance Package [] where
 -- handle f ls = map f ls                      -- aplicamos f a cada elemento de la lista (map), volvemos a devolver lista (map)
 -- handle f = map f
    handle = map                                -- es lo mismo que hacer directamente map


-- handle es map para el tipo listas
-- lo que hace es abrir la lista y aplicar la funcion a cada elemento
-- al final vuelve a poner la tapa, osea que sigue siendo una lista
-- * En este caso el map se encarga de todo, incluso del caso vacio de la lista *


-- Implementacion de Package para funciones que toman un tipo a --
-- Este es un poco mas complicado

-- Puedo ver a la funcion como un diccionario/map de clave -> valor

-- La funcion que toma algo tipo a y devuelve algo de tipo b seria:  (a -> b)
-- Se puede escribir: (->) a b, asi como se puede escribir: 5 + 6 <=> (+) 5 6
-- Entonces, las funciones que toman algo de tipo a serian: (->) a
-- (usando aplicacion parcial)

instance Package ((->)a) where
    handle f p = \k -> f (p k)                   -- asumo que me pasan la funcion (f) y el paquete a modificar (p)
 -- handle f p = \k -> (f . p) k
 -- handle f p = (f . p)
 -- handle f p = (.) f p
 -- handle = (.)

-- handle para las funciones, es la composicion

{-
EXPLICACION MAS DETALLADA
Recordar que vemos  a las funciones como maps de clave -> valor. Entonces digo que k es mi clave.
Como era el TIPO de handle?
  handle :: (a->b) -> p a -> p b
              \
            la funcion que quiero aplicar "adentro" del paquete, la voy a llamar f


                      "map"
                 /-------------\

                    2) le aplico f, que es una funcion (a -> b)
                       |
    handle f p = \k -> f (p k)
                         \___/
                       1) busco el valor de la clave k, tiene tipo a

                       \______/
                  3) f (p k) :: b    porque estoy aplicando f
                    
  4) uso \k -> al final para volver a "ponerle la tapa" y que vuelva a ser una funcion (porque las funciones son maps)
  
  Osea que lo que hice fue: aplicar la funcion f al "valor" de "adentro" del map, y volver a cerrarlo
  Que en este caso, era lo mismo que componer f con p (porque p es una funcion!)
  
  * La IDEA de handle es la misma para todos los tipos, lo que cambia es la forma de implementarlo en cada uno *
  
-}
