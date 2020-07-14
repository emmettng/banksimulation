module Answers where 

import qualified System.Random as R
import qualified Sampling as S
import qualified Randomness as CR

answers :: IO ()
answers = do 
    putStrLn "How many customers are supposed to be served ? (better larger than 100)"
    customers'number <- getLine
    arrive'list <- S.customer'arrive
    y'process'list <- S.customer'process CR.yellow
    let n'customers = read customers'number
    let yellows@(yellow'avg,yellow'max) = S.sampling arrive'list y'process'list n'customers
    print ("The average waiting of yellow customer is " ++ (show yellow'avg))
    print ("The maximum waiting of yellow customer is " ++ (show yellow'max))
    arrive'list <- S.customer'arrive
    r'process'list <- S.customer'process CR.red 
    b'process'list <- S.customer'process CR.blue 
    let (red'len'avg, red'len'max) = S.queue'length  arrive'list r'process'list n'customers
        reds@(red'avg,red'max) = S.sampling arrive'list r'process'list n'customers
        blues@(blue'avg,blue'max) = S.sampling arrive'list b'process'list n'customers
    print ("The average queue length of red customer is " ++ (show red'len'avg))
    print ("The maximum queue length of red customer is " ++ (show red'len'max))
    let tmpList = (abs . (uncurry (-)))<$>[yellows,reds,blues]
        minTmp = minimum tmpList
        tmpTuples = zip ["yellow","red","blue"] ((\x -> x-minTmp) <$> tmpList)
        min'list = filter (\(_,v)-> v==0) tmpTuples
    print ("The type of customer gives the closest value between average and maximum customer waiting times is: " ++ (show min'list))







