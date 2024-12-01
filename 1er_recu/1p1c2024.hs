{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Collapse lambdas" #-}

data AT a = NilT | Tri a (AT a) (AT a) (AT a) deriving Show

at1 = Tri 1 (Tri 2 NilT NilT NilT) (Tri 3 (Tri 4 NilT NilT NilT) NilT NilT) (Tri 5 NilT NilT NilT) 

--a)
foldAT :: (a->b->b->b->b) -> b -> AT a -> b
foldAT _ z NilT = z
foldAT f z (Tri v ri rm rd) = f v (foldAT f z ri) (foldAT f z rm) (foldAT f z rd)

--b)
preorder :: AT a -> [a]
preorder = foldAT (\v ri rm rd -> [v] ++ ri ++ rm ++rd) []

--c)
mapAT :: (a->b) -> AT a  -> AT b
mapAT f= foldAT (\v ri rm rd ->  (Tri (f v) ri rm rd)) NilT

--d)
nivel :: AT a -> Int -> [a]
nivel at n = foldAT (\v ri rm rd -> (\x -> if x /= n then (ri (x+1)) ++ (rm (x+1)) ++ (rd (x+1)) else [v])) (const []) at 0

