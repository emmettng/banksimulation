module Sampling where 

import qualified System.Random as R
import qualified Randomness as CR
import qualified Data.List as DL

type Average = Float 
type Maximum = Float 

customer'arrive :: IO [Float]
customer'arrive  = do 
    g <- R.getStdGen 
    return $ CR.arrive <$> R.randoms g

customer'process :: (Float -> Float) -> IO [Float]
customer'process  colour = do 
    g <- R.getStdGen 
    return $ colour <$> R.randoms g

-- | 
-- arrive'list : infinite list of arrive time 
-- process'list : infinite list of processing time for each customer
sampling :: [Float] -> [Float] -> Int -> (Average,Maximum)
sampling arrive'list process'list n = (avg, max)
    where 
        waiting'list = zipWith (-) process'list (tail arrive'list)
        all = sum $ filter (>=0) $ take n waiting'list
        avg = all / (fromIntegral n)
        (_,max) = max'waiting'time $ take n waiting'list

-- |
-- auxiliary function being used to calculate the average waiting time and maximum waiting time
-- I assume that the bank can only handle one customer at a time, and the following customer will be arriving 
-- after certain amount of time recorded in customer'arrive list.
max'waiting'time waiting'list = foldl get'max (0.0, 0.0) waiting'list
    where 
        get'max :: (Float,Float) -> Float -> (Float,Float)
        get'max (buf,max) elem 
            | buf + elem >= max = (buf + elem, buf + elem) 
            | buf + elem <0     = (0,max) 
            | otherwise         = (buf + elem ,max)
-- | 
-- arrive'list : infinite list of arrive time 
-- process'list : infinite list of processing time for each customer
queue'length ::[Float] -> [Float] -> Int -> (Float,Int)
queue'length arrive'list process'list n = (avg'len, maximum queuing'list)
    where 
        waiting'list = zipWith (-) process'list (tail arrive'list)
        (_,_,queuing'list) = foldl c (0,0,[]) $ take n waiting'list
        avg'len = (fromIntegral (sum queuing'list)) / (fromIntegral (length queuing'list))
        c :: (Float, Int, [Int]) -> Float -> (Float,Int, [Int])
        c (buff, cnt, lens) elem 
            | buff == 0 && elem <= 0 = (buff,cnt, lens)
            | buff == 0 && elem > 0 = (buff + elem, cnt+1, lens)
            | buff > 0 && elem < 0 = 
                if buff + elem > 0 
                    then (buff + elem, cnt+1, lens)
                    else (0, 0, (cnt+1):lens)
            | otherwise = (buff,cnt,lens)
            