
-- Paquetes contenedores de datos ----

class Package p where
    handle::(a->b) -> p a -> p b

-- ¿Que hace handle?
-- Genera un nuevo paquete de igual estructura p pero que contiene datos de tipo b.
-- En cierta forma, es como si aplicara la funcion adentro de los paquetes.

-- -----------------------------------
-- -----------------------------------


-- Implementacion de Package para el tipo Maybe --

instance Package Maybe where
    handle f (Just x) = Just (f x) 
    handle f Nothing  = Nothing
--              |
--           abrimos el tipo, osea usamos pattern matching
--           para saber que hacer en cada caso



-- Implementacion de Package para el tipo Listas --

instance Package [] where
    -- handle f ls = map f ls
    -- handle f = map f
    handle = map

-- handle es map para el tipo listas
-- lo que hace es abrir la lista y aplicar la funcion a cada elemento



-- Implementacion de Package para funciones que toman un tipo a --

-- Puedo ver a la funcion como un diccionario/map de clave -> valor

-- La funcion que toma algo tipo a y devuelve algo de tipo b seria:  (a -> b)
-- Se puede escribir: (->) a b, asi como se puede escribir: 5 + 6 <=> (+) 5 6
-- Entonces, las funciones que toman algo de tipo a serian: (->) a
-- (usando aplicacion parcial)

instance Package ((->)a) where
    handle f p = \k -> f (p k)
    -- handle f p = \k -> (f . p) k
    -- handle f p = (f . p)
    -- handle f p = (.) f p
    -- handle = (.)

-- handle para las funciones, es la composicion

{-
Como era el tipo de handle?
  handle :: (a->b) -> p a -> p b

                      "map"
                 /-------------\

                      le aplico f, que es una funcion (a -> b)
                       |
    handle f p = \k -> f (p k)
                         \---/
                         busco el valor de la clave k, tiene tipo a

                       \-----/
                    f (p k) :: b    porque estoy aplicando f
-}
