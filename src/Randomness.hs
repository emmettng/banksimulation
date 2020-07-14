module Randomness where 

arrive :: Float -> Float
arrive p = -100 * term'1  
    where term'1 = log (1-p)

yellow :: Float -> Float 
yellow = beta' 5 1

red :: Float -> Float 
red = beta' 2 2

blue :: Float -> Float 
blue = beta' 2 5

beta' :: Int -> Int -> Float -> Float
beta' alpha beta x = 200 * (term'1 * term'2)
    where 
        term'1 = x ** (fromIntegral (alpha - 1))
        term'2 = (1-x)**(fromIntegral (beta - 1))
