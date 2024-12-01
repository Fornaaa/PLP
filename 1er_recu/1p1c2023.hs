data HashSet a = Hash (a-> Integer ) (Integer -> [a])  

instance Show a => Show (HashSet a) where
    show (Hash _ felems) = show (concatMap felems [0..10]) -- Asume un rango limitado de valores de hash


vacio :: (a-> Integer ) -> HashSet a
vacio f = Hash f (const [])

hashInt :: Integer -> Integer
hashInt = id

-- Conjunto 1: {1, 2, 3}
set1 :: HashSet Integer 
set1 = agregar 3 (agregar 2 (agregar 1 (vacio hashInt)))

-- Conjunto 2: {2, 3, 4}
set2 :: HashSet Integer
set2 = agregar 4 (agregar 3 (agregar 2 (vacio hashInt)))


pertenece :: Eq a => a -> HashSet a -> Bool 
pertenece x (Hash fh felems)= elem x (felems (fh x))

agregar :: Eq a => a -> HashSet a -> HashSet a
agregar x (Hash fh felems) = if pertenece x (Hash fh felems) then (Hash fh felems)
                                else (Hash fh (\int -> if fh x == int then x:(felems int) else felems int))



interseccion :: Eq a => a -> HashSet a -> HashSet a -> HashSet a
interseccion x (Hash f1 felem1) (Hash f2 felem2) = (Hash f1 (\int -> foldr (\e rec -> if elem e (felem1 int) then e : rec else rec) [] (felem2 int)))

-- interseccionP (lo q hicieron en el parcial)
-- interseccionP :: Eq a => a -> HashSet a -> HashSet a -> HashSet a
-- interseccionP (Hash f t) c = Hash f (filter(flip pertenece c).t)

--e)
foldr1 :: (a->a->a) -> [a] -> a
foldr1 f l = if null l then error ("lista vacia") else foldr f (last l) (init l)

-- foldr toma los 2 ultimos elementos de la lista y aplica la funcion, luego tima ese resultado recursivo y lo usa para aplicarselo
-- al antepenultimo elemento con la funcion y asi
