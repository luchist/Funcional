-- ------------------------------------------------
-- ------------------------------------------------
-- Interface comun a cajas contenedoras de datos --

class Box m where
    link  :: (a -> m b) -> m a -> m b
    pack  :: a -> m a
    unite :: m (m a) -> m a

-- ------------------------------------------------

-- Implementacion de Box para el tipo Maybe

instance Box Maybe where
    pack = Just
    link f Nothing  = Nothing
    link f (Just x) = f x
    --
    unite Nothing = Nothing
    unite (Just Nothing) = Nothing
    unite (Just (Just x)) = Just x


instance Box [] where
    pack x    = [x]
    unite     = concat
    link f ls = unite (handle f ls)
    --bind f ls = concat (map f ls)


instance Box ((->)a) where
    pack v    = \k->v                  -- return
    unite b   = \k -> (b k) k          -- concat
    -- handle f b = \k -> f (b k)      -- para recordar el handle = map
    -- link f b  = unite (handle f b)  -- el bind es el concatMap
    link f b  = \k-> (f (b k)) k


handle' :: Box m => (a -> b) -> m a -> m b
handle' f m = link f' m
                where f' x = pack (f x)


unite' :: Box m => m (m a) -> m a
unite' m = link f m
             where f x = f x

employ ::  Box m => m (a -> b) -> m a -> m b
employ mf m = link f mf
                where f x = handle' x m

{-
    link  :: (a -> m b) -> m a -> m b -- (>>=)
    pack  :: a -> m a                 -- return
-}

associate :: Box m => (a -> m b) -> [a] -> m [b]
-- TODO
-- associate f ls = pack (handle' f ls)