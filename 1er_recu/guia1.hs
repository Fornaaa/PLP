

--Ejercicio 1

max2 :: Ord a => (a,a) -> a
max2 (x, y) | x >= y = x 
            | otherwise = y

subtract :: Int ->Int -> Int
subtract = flip (-)

normaVectorial :: (Float,Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)

predecesor :: Int -> Int
predecesor = Main.subtract 1


evaluarEnCero :: (Int -> a) -> a
evaluarEnCero = \f-> f 0

dosVeces :: (a->a) -> (a->a)
dosVeces = \f-> f . f

composicion :: (b->c) -> (a->b) -> (a->c)
composicion = \f g -> f . g

flipAll :: [a -> b -> c]->[b->a->c]
flipAll = Prelude.map flip

flipRaro :: b-> (a->b->c) -> a -> c
flipRaro = flip flip

-- flip flip a -> b ->c
-- flip (a->b->c) ->  (b->a->c)
--flip flip a b
-- flip ((a->b->c) -> b -> a -> c)

--Ejercicio 2 
--a)
curry :: ((a,b) ->c) -> a -> b -> c
curry f a b= f (a,b) 

uncurry :: (a-> b -> c) ->(a,b) ->c
uncurry f (a,b) = f a b 

--Ejercicio 3 
--para no olvidarse 
-- foldr :: (a->b->b) -> b -> [a] -> b
-- foldr _ z [] = z 
-- foldr f z (x:xs) = f x (Main.foldr f z xs) 

sum :: [Float] -> Float
sum = foldr (+) 0

-- sum [1,2,3,4,5] [1+(2+(3+(4+(5+0))))] -> [1+(2+(3+(4+(5))))]->[1+(2+(3+(9)))]->... -> (1+14) = 15.

--elem verifica si existe elemento en lista
elem :: Eq a => a -> [a] -> Bool
elem e = foldr (\a rec -> rec || a==e) False

-- (++) :: [a] -> [a] -> [a]
-- (++) a b= foldr (:) b a 

-- a=[1,2,3] b=[4,5,6] [1:[2:[3:b]]] // b es el caso base   --> [1:[2:[3:[4,5,6]]]

--filter devuelve los elementos que cumplen la condicion dada
filter :: (a->Bool)->[a]-> [a] 
filter cond =  foldr (\a rec -> if cond a then a:rec else rec) [] 

-- map :: (a->b) -> [a] -> [b]
-- map f = foldr (\a rec -> f a: rec )  [] 

--II)
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\a rec -> if (f a rec) then a else rec) 

-- foldr1 :: (a->a->a) -> [a] -> a
-- foldr1 _ [a] = a 
-- foldr1 f (x:xs) = f x (Main.foldr1 f xs) 

sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\acc a -> if null acc then [a] else acc ++ [a + last acc]) []

-- foldl ::  (b->a->b) -> b -> [a] -> b       
-- foldl f ac [] = ac 
-- foldl f ac (x:xs) = Main.foldl f (f ac x) xs

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0

sumaAlt2 :: Num a => [a] -> a
sumaAlt2 n = sumaAlt (reverse n)

--4)
--I)
concatMap2 f = concat . map f
--Honi
permutaciones :: [a] -> [[a]]
permutaciones =
    foldr (\x rs -> concatMap (\r -> map (insert x r) [0..length r]) rs) [[]]
        where insert x r i = drop i r ++ [x] ++ take i r

--II) 
partes :: [a] -> [[a]]
partes = foldr (\x rec -> map (x:) rec ++ rec) [[]]

--III)
sufijos :: [a] -> [[a]]
sufijos = foldr (\x rec -> (x : head rec) : rec) [[]]

prefijos :: [a] -> [[a]]
prefijos = foldl (\acc x -> acc ++ [last acc ++ [x]] ) [[]]

--IV)
sublistas :: [a] -> [[a]]
sublistas = foldl (\acc x -> acc ++ map (\y -> y ++ [x]) acc) [[]]


