type Point = (Float, Float)

type Rect = (Point, Point)

data QuadT = QEmpty | 
             QLeaf Point |
             QNode Rect QuadT QuadT QuadT QuadT

                        --   ---- ----
data Quadrant = Q1 | Q2 | Q3 | Q4

quad :: Quadrant -> Rect -> Rect
quad q1 ((x0, y0), (xn, yn)) = ((x0, y0), (x2, y2))
quad q2 ((x0, y0), (xn, yn)) = ((x2, y0), (xn, y2))
quad q3 ((x0, y0), (xn, yn)) = ((x0, y2), (x2, yn))
quad q4 ((x0, y0), (xn, yn)) = ((x2, y2), (xn, yn))
    where x2 = (x0 + xn) / 2
          y2 = (y0 + yn) / 2

inside :: Rect -> Point -> Bool
inside ((x0, y0), (xn, yn)) (x, y) = (x0 <= x) && (x <= xn) &&
                                     (y0 <= y) && (y <= yn)

find :: Point -> QuadT -> Bool
find p QEmpty    = False
find p (QLeaf q) = p == q
find p (QNode r q1 q2 q3 q4) 
    | inside (quad q1 r) p = find p q1
    | inside (quad q2 r) p = find p q2
    | inside (quad q3 r) p = find p q3
    | inside (quad q4 r) p = find p q4
    | otherwise            = False

insert :: Point -> QuadT -> QuadT
insert p QEmpty    =
insert p (QLeaf q) 
    | p == q = q
    | otherwise = insertAll (empty (bound p q) (p,q))
insert p (QNode r q1 q2 q3 q4) 
    | inside (quad q1 r) p = insert p (insert p q1) q2 q3 q4
    | inside (quad q2 r) p =
    | inside (quad q3 r) p =
    | inside (quad q4 r) p =
    | otherwise            = insertAll QEmpty (p: points q)

empty r = QNode r QEmpty QEmpty QEmpty QEmpty

delete :: Point -> QuadT -> QuadT
delete p QEmpty = QEmpty
delete p (QLeaf q) 
    |
    |
delete p q@(QNode r q1 q2 q3 q4) 
    | inside (quad q1 r) p = fix (QNode r (delete p q1) q2 q3 q4)
    | inside (quad q2 r) p = fix (QNode r q1 (delete p q2) q3 q4)
    | inside (quad q3 r) p = fix (QNode r q1 q2 (delete p q3) q4)
    | inside (quad q4 r) p = fix (QNode r q1 q2 q3 (delete p q1))
    | otherwise = q

fix :: QuadT -> QuadT
fix (QNode _ q1 QEmpty QEmpty QEmpty) = q1
fix (QNode _ QEmpty q2 QEmpty QEmpty) = q2
fix (QNode _ QEmpty QEmpty q3 QEmpty) = q3
fix (QNode _ QEmpty QEmpty QEmpty q4) = q4
fix q = q

-----------------------------------------------------------------------------
------------------ 2da parte ------------------------------------------------

foldQT :: b -> (Point -> b) -> (Rect -> b -> b -> b -> b -> b) -> QuadT -> b
foldQT fe fl fn QEmpty    = e
foldQT fe fl fn (QLeaf p) = fl p
foldQT fe fl fn (QNode r q1 q2 q3 q4) = fn r (foldQT fe fl fn q1) (foldQT fe fl fn q2) 
                                             (foldQT fe fl fn q3) (foldQT fe fl fn q4)

points :: QuadT -> [Point]
points = foldQT [] (\p -> [p]) (\_ r1 r2 r3 r4 -> r1 ++ r2 ++ r3 ++ r4)

insertAll QuadT -> [Point] -> QuadT
insertAll = foldr insert

super :: [QuadT] -> QuadT
super = foldr (\q qr -> insertAll qr (points q)) QEmpty

winsurf :: QuadT -> Float
winsurf = foldQT 0 (\p -> 0) (\r q1 q2 q3 q4 -> ([surf r, q1, q2, q3, q4 // [0]]))
    where surf ((x0, y0), (xn, yn)) = (xn - x0) * (yn - y0)

collide :: Rect -> QuadT -> Bool
collide r@((x0, y0), (xn, yn)) = foldQT False (inside r) (\r c1 c2 c3 c4 -> check (quad q1 r) c1 ||
                                                                            check (quad q2 r) c2 ||
                                                                            check (quad q3 r) c3 ||
                                                                            check (quad q4 r) c4)
    where check r c = any (inside r c) (vertex r) && c
          vertex ((x0, y0), (xn, yn)) = [(x0, y0), (x0, yn), (xn, y0), (xn, yn)]