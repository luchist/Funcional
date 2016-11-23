-- Interpreter Monadico

{--
class Monad m where 
	return :: a -> m a
	(>>=) :: m a -> (a -> m b) -> m b
-}
type Variable = String
type Store = Variable -> Int

newtype Interpreter v = IntImp {step :: Store -> (Store, v)} 

-- record notation

-- Es lo mismo que hacer:
-- newtype Interpreter v = IntImp (Store -> (Store, v))

-- step :: Interpreter v -> (Store -> (Store, v))
-- step (IntImp f) = f
-- step "abre" y le saca el constructor


-- No me sale el bind!!  D:

instance Monad Interpreter where
   return x = IntImp(\s -> (s,x)) 
   m >>= k = k (snd           ((step m)         store))
{-                     -- Store -> (Store,v)   store::Store 
              snd                      (Store,v)
                             v
 lo que hago es              k v    
-}

read' :: Variable -> Interpreter Int
read' v = IntImp(\s -> (s, s v))

write' :: Variable -> Int -> Interpreter()
write' v n = IntImp(\s -> (extender s v n,())) -- \v -> \n -> (s, s v+1) 
-- hay que "guardar" la relacion v -> n en el store

extender s v n = \v' -> if v==v' then n else s v'

--memVacia ::
blanco :: Interpreter()
blanco = IntImp(\str -> (str,()))

store :: Store
store = \" " -> 0

--(write' "a" 5 blanco)
-- read' "a" blanco         --> 5

-- Uso
incX = do v <- read' "x";
               write' "x" (v+1)

{-
do v <- read' "x";              read' "x" >>= \v ->
   write' "x" (v+1);            write "x" (v+1)
-}