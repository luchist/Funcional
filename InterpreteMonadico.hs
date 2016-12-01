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

-----------------------------------------------------------------------------
-- http://aprendehaskell.es/content/MasMonadas.html#creando-monadas
-- aca esta la monada de Fidel adaptada

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

-- >>= :: Interpreter a ->  (a -> Interpreter b) -> Interpreter b

instance Monad Interpreter where
   return x = IntImp(\s -> (s,x)) 
   (IntImp h) >>= f = IntImp $ \s -> let (a, newState) = h s
                                        (IntImp g) = f a
                                        in  g newState  -- expresion
{-                        -- Store -> (Store,v)   store::Store 
              snd                      (Store,v)
                             v
 lo que hago es              k v    
-}

read' :: Variable -> Interpreter Int
read' v = IntImp(\mem -> (mem, mem v))


write' :: Variable -> Int -> Interpreter ()
write' v n = IntImp(\mem -> (assign mem v n,())) -- \v -> \n -> (s, s v+1) 
-- hay que "guardar" la relacion v -> n en el store

assign s v n = \v' -> if v==v' then n else s v'

--memVacia ::
blanco :: Interpreter()
blanco = IntImp(\str -> (str,()))

store :: Store
store = \" " -> 0


--(write' "a" 5 blanco)
-- read' "a" blanco         --> 5


-- Uso
incX = do v <- read' "x"
          write' "x" (v+1)

{-
do v <- read' "x";              read' "x" >>= \v ->
   write' "x" (v+1);            write "x" (v+1)
-}



getState :: Inte s
getState = State $ \s -> (s,s)

putState :: s -> State s ()
putState newState = State $ \s -> ((),newState)

type Memoria = Variable -> Int

vacio :: Memoria
vacio = id

assing :: Variable -> Int -> Memoria -> Memoria
assing v x m = extender v x m

read :: Variable -> Memoria -> Int
read v m = m v

vacioState = return vacio

ejemplo1 = 
    do
        mem <- getState
        mem <- putState (assing "x" 3 mem)
        if read "x" mem > 0
           return True
        else
           return False

read' :: Variable -> State Memoria Int
read' var = do
    mem <- getState
    return (mem var)

write' :: Variable -> Int -> State Memoria ()
write' var n =
    do
        mem <- getState
        putState (assing var n mem)

ejemplo2 = do
    n <- read' "x"
    write' "x" (n+1)
    n <- read' "x"
    return n


runStore :: Interpreter a -> Store -> b
runStore i s = snd ((step i) s)

execStore :: Interpreter a -> Store -> Store
execStore i s = fst ((step i) s)






type Memoria = [(Variable, Int)]

vacio :: Memoria
vacio = []

assing :: Variable -> Int -> Memoria -> Memoria
assing v x m = (v, x) : m

read :: Variable -> Memoria -> Int
read v []  = 0
read v ((v', x) : m) = 
    if v == v'
       then x
       else read v m

