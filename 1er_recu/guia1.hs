--Ejercicio 1
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Redundant bracket" #-}

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
elem1 :: Eq a => a -> [a] -> Bool
elem1 e = foldr (\a rec -> rec || a==e) False

-- (++) :: [a] -> [a] -> [a]
-- (++) a b= foldr (:) b a 

-- a=[1,2,3] b=[4,5,6] [1:[2:[3:b]]] // b es el caso base   --> [1:[2:[3:[4,5,6]]]

--filter devuelve los elementos que cumplen la condicion dada
-- filter :: (a->Bool)->[a]-> [a]
-- filter cond =  foldr (\a rec -> if cond a then a:rec else rec) []

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


--recr
recr :: (a->[a]->b->b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

trim :: String -> String
trim = recr (\x xs rec -> if x==' ' then rec else x : xs) []

data AB a = Nil | Bin (AB a) a (AB a) deriving Show

foldAB :: (a -> b -> b -> b ) -> b -> AB a -> b
foldAB _ z Nil = z
foldAB f z (Bin izq val der) = f val (foldAB f z izq) (foldAB f z der)

recrAB :: (a->AB a -> AB a-> b -> b -> b) -> b -> AB a -> b
recrAB _ z Nil = z
recrAB f z (Bin izq val der) = f val izq der (recrAB f z izq) (recrAB f z der)



mapAB :: (a->b) -> AB a -> AB b
mapAB f = foldAB (\val izq der -> (Bin izq (f val) der)) Nil

data ArbolBinario a = Hoja a | Nodo (ArbolBinario a) a  (ArbolBinario a)
                  deriving (Show, Eq)

foldAB2 :: (a -> b -> b -> b) -> (a->b) -> ArbolBinario a -> b
foldAB2 _ fz (Hoja a) = fz a
foldAB2 f fz (Nodo i val d) = f val (foldAB2 f fz i) (foldAB2 f fz d) 

mapAB2 :: (a->b) -> ArbolBinario a -> ArbolBinario b
mapAB2 fz = foldAB2 (\val izq der -> (Nodo izq (fz val) der)) (\x -> Hoja (fz x))

-- mapE :: (a->b) -> AB a -> AB b
-- mapE f (Bin izq val der) = Bin (mapE f izq) (f val) (mapE f der)


--foldBuffer
data Buffer a = Empty | Write Int a (Buffer a) | Read Int (Buffer a) deriving (Eq, Show)
-- b0 = Write 1 ''a'' $ Write 2 ''b'' $ Write ''c'' $ Empty
-- b1 = Write 2 True $ Empty
-- b2 = Write 2 True $ Write 2 False $ Empty  
-- b3 = Read 1 $ Write 2 True $ Write 1 False $ Empty  


-- Parcial 1 2C 2024
foldBuffer :: (Int -> a -> b -> b) -> (Int -> b -> b) -> b ->  Buffer a -> b
foldBuffer _ _ z Empty = z
foldBuffer fw fr z (Write ind elem buf) = fw ind elem (foldBuffer fw fr z buf)
foldBuffer fw fr z (Read ind buf) = fr ind (foldBuffer fw fr z buf)

recrBuffer :: (Int -> a -> Buffer a -> b -> b) -> (Int -> Buffer a -> b -> b) -> b ->  Buffer a -> b
recrBuffer _ _ z Empty = z
recrBuffer fw fr z (Write ind elem buf) = fw ind elem buf (recrBuffer fw fr z buf)
recrBuffer fw fr z (Read ind buf) = fr ind buf (recrBuffer fw fr z buf)

-- Read 1 | Write 2 "chau"| Write 1 "hola" | Write 1 "que" | Empty

--b)
posicionesOcupadas :: Buffer a -> [Int]
posicionesOcupadas = foldBuffer (\ind _ rec -> if elem ind rec then rec else rec ++ [ind]) (\ind rec -> filter (\x -> x /= ind) rec) []

--c)
contenido :: Int -> Buffer a -> Maybe a
contenido n = foldBuffer (\ind a rec -> if ind == n then Just a else rec) (\ind rec -> if ind == n then Nothing else rec) Nothing 

--d)
puedeCompletarLecturas :: Buffer a -> Bool
puedeCompletarLecturas = recrBuffer (\_ _ _ rec -> rec) (\ind buf rec -> if rec && elem ind (posicionesOcupadas buf) then rec else False) True 

--e)
deshacer :: Buffer a -> Int -> Buffer a
deshacer = recrBuffer (\ind a buf rec -> (\n -> if n/=0 then (rec (n-1)) else (Write ind a buf))) (\ind buf rec -> \n -> if n/=0 then rec (n-1) else Read ind buf) (const Empty)

-- deshacer :: Buffer a -> Int -> Buffer a
-- deshacer buf 0 = buf
-- deshacer Empty _ = Empty
-- deshacer (Write ind elem buf) n = deshacer buf (n-1)
-- deshacer (Read ind buf) n = deshacer buf (n-1)

