-- ------------------------------------------------
-- ------------------------------------------------
-- Interface comun a cajas contenedoras de datos --

class Box m where
    link  :: (a -> m b) -> m a -> m b
    pack  :: a -> m a
    unite :: m (m a) -> m a               -- es para definir mas facilmente el link

-- ------------------------------------------------

-- Implementacion de Box para el tipo Maybe --

instance Box Maybe where
    pack = Just
    link f Nothing  = Nothing
    link f (Just x) = f x

    --
    unite Nothing = Nothing
    unite (Just Nothing) = Nothing
    unite (Just (Just x)) = Just x


-- Implementacion de Box para el tipo Lista --

instance Box [] where
    pack x    = [x]
    unite     = concat
    link f ls = unite (map f ls)
    --bind f ls = concat (map f ls)


-- Implementacion de Box para las funciones que toman un tipo a --

instance Box ((->)a) where
    pack v    = \k -> v                       -- return
    unite b   = \k -> (b k) k                 -- concat
    -- handle f b = \k -> f (b k)             -- map, definido en Package
    -- link f b  = unite (handle f b)         -- el bind es el concatMap
    link f b  = \k-> (f (b k)) k
-- ----------------------------------------------------------




handle' :: Box m => (a -> b) -> m a -> m b
handle' f m = link f' m
                where f' x = pack (f x)

{-
Es el equivalente a liftM

liftM :: (Monad m) => (a -> b) -> (m a -> m b)
liftM f m  = do  x <- m 
                 return (f x)


liftM "promueve" una funcion a una monada.
Puede verse como que le das una funcion y una monada, y aplica 
la funcion "adentro" de la monada:

liftM :: (Monad m) => (a -> b) ->  m a -> m b

o bien se puede ver como una funcion a la cual le pasas una funcion (a -> b)
y te devuelve una funcion monadica (m a -> m b)

liftM :: (Monad m) => (a -> b) -> (m a -> m b)

--
Otras versiones:
--

-- Del modulo monad.hs, notacion do con corchetes y ; --
liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM f m1  = do { x1 <- m1; return (f x1) }


-- Si bind tomara primero la monada y despues la funcion, como en las diapositivas --
liftM f m = (>>=) m (\x ->
            return (f x) )

-- Version con where --
liftM f m = bind f' m
               where f' x =  return (f x)

-- Version con funcion anonima (lambda)
liftM f m = bind (\x -> return (f x)) m

-}

-- Version que promueve una funcion que toma dos argumentos

{-
liftM2  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r

liftM2 f m1 m2          = do  x1 <- m1
                              x2 <- m2
                              return (f x1 x2)


liftM2 f m1 m2 = (>>=) m1 (\x1 ->
                 (>>=) m2 (\x2 ->
                 return f x1 x2))

liftM2 f m1 m2 = (>>=) (\x1 -> (>>=) (\x2 -> return f x1 x2) m2 ) m1
-}

handle2' :: Box m => (a -> b -> c) -> m a -> m b -> m c
handle2' f m1 m2  = link (\x -> link (\y -> pack (f x y) ) m2 ) m1


-- ----------------------------------------------------------


unite' :: Box m => m (m a) -> m a
unite' m = link f m
             where f x = f x

unite'' :: Box m => m (m a) -> m a
unite'' m = link id m

-- Es el equivalente a la funcion join
-- join :: (Monad m) => m (m a) -> m a
-- join x =  x >>= id

-- join x = bind id x

{-
join se usa para remover un nivel de estructura monadica,
proyectando su argumento ligado hacia afuera
-}


-- ----------------------------------------------------------

-- Es el equivalente a la funcion ap
-- ap :: (Monad m) => m (a -> b) -> m a -> m b
-- ap = liftM2 id
employ ::  Box m => m (a -> b) -> m a -> m b
employ mf m = link f mf
                where f x = handle' x m

-- ap es similar a liftM
-- TODO diferencias entre ap y liftM

-- ----------------------------------------------------------

-- Dada una funcion que construye cajas y una lista de elementos,
-- retorna una caja de listas


associate :: Box m => (a -> m b) -> [a] -> m [b]
associate f ls  = sucession (map f ls)  
-- associate f = sucession . map f

-- Es el equivalente a mapM 
-- mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
-- mapM f = sequence . map f



-- ----------------------------------------------------------


-- Dada una lista de cajas, construye una unica caja con la lista de elementos 
-- resultante de recolectar secuencialmente los elementos de las cajas
-- 
-- La funcion es recursiva en la estructura de las listas

sucession :: (Box m) => [m a] -> m [a]
sucession = foldr (handle2' (:)) (pack [])

-- sucession []     = pack []                             -- armo una caja con la lista vacia
-- sucession (m:ms) = (handle2' (:)) m  (sucession ms)    -- lifteo cons

{-
Es el equivalente a sequence:

sequence       :: Monad m => [m a] -> m [a]
sequence []     = return []
sequence (c:cs) = do   x  <- c
                       xs <- sequence cs
                       return (x:xs)

                       c >>= \x            ->
                       sequence cs >>= \xs ->
                       return (x:xs)


-- otra definiciones de sequence:

sequence = foldr (liftM2 (:)) (return [])
-}