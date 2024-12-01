{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
import Data.List
import Control.Lens.Combinators (Field1(_1))
data Prop = Var String | No Prop | Y Prop Prop | O Prop Prop | Imp Prop Prop

type Valuacion = String -> Bool

expr = Y (Var "p") (No (Imp (Var "Q") (Var "R")))

--foldProp :: (Prop->b->b) -> (Prop -> Prop -> b->b) -> (Prop -> Prop -> b->b) -> (Prop -> Prop -> b->b) -> (String -> b) -> Prop a-> b
--foldProp:: (b->b)->(b->b->b)->(b->-b->b)->(b->b->b)->(String->b)->Prop  -> b

--a)
foldProp :: (t -> t) -> (t -> t -> t) -> (t -> t -> t) -> (t -> t -> t) -> (String -> t) -> Prop -> t
foldProp fn fy fo fim fz (Var z) = fz z
foldProp fn fy fo fim fz (No p) = fn (rec p) where rec = foldProp fn fy fo fim fz
foldProp fn fy fo fim fz (Y p q) = fy (rec p) (rec q) where rec = foldProp fn fy fo fim fz
foldProp fn fy fo fim fz (O p q) = fo (rec p) (rec q) where rec = foldProp fn fy fo fim fz
foldProp fn fy fo fim fz (Imp p q) = fim (rec p) (rec q) where rec = foldProp fn fy fo fim fz

recrProp :: (t -> Prop -> t) -> (t -> t -> Prop -> Prop -> t) -> (t -> t -> Prop -> Prop -> t) -> (t -> t -> Prop -> Prop -> t) -> (String -> t) -> Prop -> t
recrProp fn fy fo fim fz (Var z) = fz z
recrProp fn fy fo fim fz (No p) = fn (rec p) p 
            where rec = recrProp fn fy fo fim fz
recrProp fn fy fo fim fz (Y p q) = fy (rec p) (rec q) p q 
            where rec = recrProp fn fy fo fim fz
recrProp fn fy fo fim fz (O p q) = fo (rec p) (rec q) p q 
            where rec = recrProp fn fy fo fim fz
recrProp fn fy fo fim fz (Imp p q) = fim (rec p) (rec q) p q 
            where rec = recrProp fn fy fo fim fz

--b)
variables :: Prop -> [String]
variables = foldProp (\rec -> rec) (\rec1 rec2 -> (union rec1 rec2)) (\rec1 rec2 -> (union rec1 rec2)) (\rec1 rec2 -> (union rec1 rec2)) (\var -> [var])

--c)
evaluar :: Valuacion -> Prop -> Bool
evaluar fval = foldProp (\x -> not x) (\x1 x2 -> x1 && x2) (\x1 x2 -> x1 || x2) (\x1 x2 -> not x1 || x2) (\var -> fval var) 

--d)


estaEnFNNexp :: Prop -> Bool 
estaEnFNNexp (Var p) = True 
estaEnFNNexp (No (Var _)) = True
estaEnFNNexp (No _) = False
estaEnFNNexp (Y p q) = estaEnFNNexp p && estaEnFNNexp q
estaEnFNNexp (Imp _ _) = False

estaEnFNN :: Prop -> Bool 
estaEnFNN = recrProp (\rec p -> rec && (isVar p)) (\rec1 rec2 _ _-> rec1 && rec2) (\rec1 rec2 _ _ -> rec1 && rec2) (\_ _ _ _-> False) (\_ -> True)

isVar :: Prop -> Bool
isVar (Var _) = True
isVar _ = False