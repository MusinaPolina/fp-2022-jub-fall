{-# LANGUAGE FlexibleInstances #-}
module Lambda where

import Data.List as List

-- Тип для лямбда-термов.
-- Параметризуем типом переменной.
data Lambda a = Var a
              | App (Lambda a) (Lambda a)
              | Abs a (Lambda a)
              deriving (Eq)

-- true ≡ λx.λy.x
true = Abs "x" (Abs "y" (Var "x"))

-- false ≡ λx.λy.y
false = Abs "x" (Abs "y" (Var "y"))

-- and ≡ λp.λq.p q p
and = Abs "p" (Abs "q" (App (App (Var "p") (Var "q")) (Var "p")))

-- or ≡ λp.λq.p p q
or = Abs "p" (Abs "q" (App (App (Var "p") (Var "p")) (Var "q")))

-- not ≡ λp.p FALSE TRUE
not = Abs "p" (App (App (Var "p") false) true)

-- ifThenElse ≡ λp.λa.λb.p a b
ifThenElse = Abs "p" (Abs "a" (Abs "b" (App (App (Var "p") (Var "a")) (Var "b"))))

-- zero ≡ λf.λx.x
zero = Abs "f" (Abs "x" (Var "x"))

-- one ≡ λf.λx.f x
one = Abs "f" (Abs "x" (App (Var "f") (Var "x")))

-- two ≡ λf.λx.f (f x)
two = Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (Var "x"))))

-- three ≡ λf.λx.f (f (f x))
three =  Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))

--  four ≡ λf.λx.f (f (f (f x)))
four =  Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))))

-- add ≡ λm.λn.λf.λx.m f (n f x)
add = Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "m") (Var "f")) (App (App (Var "n") (Var "f")) (Var "x"))))))

-- successor ≡ λn.λf.λx.f (n f x)
successor = Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x")))))

-- add' ≡ λm.λn.m successor n
add' = Abs "m" (Abs "n" (App (App (Var "m") successor) (Var "n")))

-- mult ≡ λm.λn.λf.m (n f)
mult = Abs "m" (Abs "n" (Abs "f" ((App (Var "m") (App (Var "n") (Var "f"))))))

-- mult' ≡ λm.λn.m (add n) 0
mult' = Abs "m" (Abs "n" (App (App (Var "m") (App add (Var "n"))) zero))

-- Красивая печать без лишних скобок.
wrap :: Bool -> String -> String
wrap t s = if t then "(" ++ s ++ ")" else s

isAbs :: Lambda a -> Bool
isAbs (Abs _ _) = True
isAbs _ = False

isVar :: Lambda a -> Bool
isVar (Var _) = True
isVar _ = False

instance {-# OVERLAPS #-} Show (Lambda String) where
  show (Var a) = a
  show (Abs a t) = "λ" ++ a ++ "." ++ show t
  show (App a b) = wrap (isAbs a) (show a) ++ " " ++ wrap (Prelude.not $ isVar a) (show b)

instance {-# OVERLAPPABLE #-} Show a => Show (Lambda a) where
  show l = case l of
    Var a -> show a
    Abs a t -> "λ" ++ (show a) ++ "." ++ show t
    App a b -> wrap (isAbs a) (show a) ++ " " ++ wrap (Prelude.not $ isVar a) (show b)
  
  

-- Выберите подходящий тип для подстановок.
data Subst a = Subst a (Lambda a)

-- Проверка термов на альфа-эквивалентность.
alphaEq :: Eq a => Lambda a -> Lambda a -> Bool
alphaEq = undefined

-- Capture-avoiding substitution.
cas :: Lambda a -> Subst a -> Lambda a
cas = undefined

-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

-- Интерпретатор лямбда термов, учитывающий стратегию.
eval :: Strategy -> Lambda a -> Lambda a
eval = undefined

-- ДеБрауновское представление лямбда-термов
data DeBruijn = VarDB Int
              | AbsDB DeBruijn
              | AppDB DeBruijn DeBruijn

isAbsDB :: DeBruijn -> Bool
isAbsDB (AbsDB _) = True
isAbsDB _ = False

isVarDB :: DeBruijn -> Bool
isVarDB (VarDB _) = True
isVarDB _ = False

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show d = case d of
    VarDB a -> show a
    AbsDB a -> "λ" ++ (show a)
    AppDB a b -> wrap (isAbsDB a) (show a) ++ " " ++ wrap (Prelude.not $ isVarDB a) (show b)

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Eq a => Lambda a -> DeBruijn
toDeBruijn lambda = convert lambda []
  where   
    convert :: Eq a => Lambda a -> [a] -> DeBruijn
    convert (Var a) lst = 
      case (elemIndex a lst) of
          Just index -> VarDB index
          Nothing -> undefined 
    convert (Abs a t) lst = AbsDB (convert t (a : lst))
    convert (App a b) lst = AppDB (convert a lst) (convert b  lst)

alphabet = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", 
            "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"] 

-- -- Преобразовать деБрауновские лямбда-термы в обычные.
-- fromDeBruijn :: DeBruijn -> Lambda a
-- fromDeBruijn db = 
--   let numberOfVariables = maxNumber db in
--   let variables = generateVariables [""] numberOfVariables in
--   first convert db variables
--   where
--     maxNumber :: DeBruijn -> Int
--     maxNumber (VarDB a) = a 
--     maxNumber (AbsDB a) = maxNumber a 
--     maxNumber (AppDB a b) = max (maxNumber a) (maxNumber b)

--     generateVariables :: [String] -> Int -> [String]
--     generateVariables lst n = 
--       if length lst <= n then extend lst
--       else lst

--     extend :: [String] -> [String]
--     extend [] = []
--     extend (x : xs) = (map (++ x) alphabet) ++ extend xs

--     convert :: DeBruijn -> [String] -> (Lambda a, Int)
--     convert (VarDB a) v = (Var (v !! a), 1)
--     convert (AbsDB t) v = Abs (convert t v)
--     convert (AppDB a b) v = App (convert a v) (convert b v)
