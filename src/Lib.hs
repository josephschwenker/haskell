module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "Hello world"

add :: Num a => a -> a -> a
add x y = x + y

-- >>> add 2 3
-- 5

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"
    where
        bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0


max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

-- >>> 3 `myCompare` 2
-- GT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where
        (f:_) = firstname  
        (l:_) = lastname    

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height^2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2*pi*r*h
        topArea = pi*r^2
    in sideArea + 2*topArea

-- data Bool = False | True

data Shape = Circle Float Float Float | Rectangle Float Float Float Float

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r^2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- >>> max 4 5
-- 5

multThree :: Num a => a -> a -> a -> a
multThree x y z = x*y*z

-- >>> multThree 3 5 9
-- 135

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = ( `elem` ['A'..'Z'] )