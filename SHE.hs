module SHE
( 
polyFunc
, polyFullMult
--, gghEnc
--, gghDec
, rlweKeyGen
, discreting
, rlweSK
, a1Gen 
, a0Gen
, eGen
--, rlwePK
) where 

import Data.Matrix
import qualified Data.Random.Normal as N 
import System.Random

polyFunc f (x:xs) (y:ys) = [f x y] ++ polyFunc f xs ys
polyFunc f [] (y:ys) = [0] ++ polyFunc f [] ys
polyFunc f (x:xs) [] = [0] ++ polyFunc f xs []
polyFunc f [] [] = []

polyFullMult (x:xs) (y:ys) = polyFunc (+) (map (*x) (y:ys)++(replicate (length xs) 0)) ([0]++polyFullMult xs (y:ys))
polyFullMult [] (y:ys) = replicate (length ys) 0
polyFullMult (x:xs) [] = []
polyFullMult [] [] = []

----------------------------------------------------------------------------------------------------------------------------------------------------------------------
     
-- The code here is just for testing with the GGH crpytosystem
     
--gghEnc m = elementwise (+) (multStd basis (fromList 4 1 m)) error
--	where basis = fromLists [[1,0,0,0],[0,1,0,0],[0,0,1,0],[-349,311,321,851]]
--	      error = fromList 4 1 [-1,1,1,-1]
		  		  
--gghDec c = multStd (multStd publicInv private) (fromList 4 1 $ map (fromIntegral) $ map (round) $ toList (multStd privateInv c)) 
--    where publicInv = fromLists [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0.4101058,-0.3654524,-0.37772033,0.001175088]] 
--          private = fromLists [[3,1,-1,3],[3,-4,3,1],[3,-3,-4,-3],[-2,-3,-2,3]] 
--          privateInv = fromLists [[0.14101058,0.06345476,0.06227967,-0.09988249],[0.09048179,-0.10928320,-0.05170388,-0.10575793],[-0.06698002,0.11985899,-0.10458284,-0.07755582],[0.13983549,0.01292597,-0.07990599,0.10928320]]

----------------------------------------------------------------------------------------------------------------------------------------------------------------------
		  
rlweKeyGen n q sig t = [n] ++ [q] ++ [sig] ++ [t] 
          
discreting x = map (round) x        
          
rlweSK r n
    | n <= 0 = [] 
    | otherwise = fst (N.normal (mkStdGen r)):(rlweSK (r+1) (n-1))
    
a1Gen r q n = 
    | n <= 0 = []
    | otherwise = fst $ randomR (0,q) (mkStdGen r):(a1Gen (r+1) q (n-1))

a0Gen a1 r2 r3 q n t = map (*(-1)) (polyFunc (+) (polyFullMult a1 (rlweSK r2 n)) (map (*t) (eGen r3 n)))

eGen r n
    | n <= 0 = [] 
    | otherwise = fst (N.normal (mkStdGen r)):(rlweSK (r+1) (n-1))
       
rlwePK r1 r2 r3 q n t = [a0,a1]
    where a1 = a1Gen r1 q n 
          a0 = a0Gen a1 r2 r3 q n t
