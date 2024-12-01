data Componente = Contenedor | Motor | Escudo | Cañon deriving (Eq, Show)  
data NaveEspacial = Modulo Componente NaveEspacial NaveEspacial | Base Componente deriving (Eq,Show) 


recNave :: (Componente -> b -> b -> NaveEspacial -> NaveEspacial -> b) -> (Componente-> b) -> NaveEspacial -> b
recNave _ fb (Base comp)= fb comp
recNave fm fb (Modulo comp n1 n2) = fm comp (recNave fm fb n1) (recNave fm fb n2) n1 n2

foldNave :: (Componente -> b -> b -> b) -> (Componente-> b) -> NaveEspacial -> b
foldNave fm fb = recNave (\comp rec1 rec2 _ _ -> fm comp rec1 rec2) fb 

--b)
espejo :: NaveEspacial -> NaveEspacial
espejo = foldNave (\comp r1 r2 -> Modulo comp r2 r1) (\comp -> Base comp)

--c)
esSubNavePropia :: NaveEspacial -> NaveEspacial -> Bool
esSubNavePropia nav1 = recNave (\comp r1 r2 n1 n2 -> nav1== n1 || nav1==n2 || r1 || r2) (const False)

-- (base Motor) (Modulo Motor (Base Motor) (Base Contenedor))

--d) 
truncar :: NaveEspacial -> Integer -> NaveEspacial
truncar = foldNave (\comp r1 r2 -> (\n -> if n==0 then (Base comp) else (Modulo comp (r1 (n-1)) (r2 (n-1))))) (\x -> \i-> Base x) 

