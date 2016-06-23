module SHE
( 
polyFunc
, polyFullMult
, zeroRem
, leadDiv
, tupAdds
, polyMod
, gcdExt
, modInv
--, gghEnc
--, gghDec
, rlweKeyGen
, discreting
, rlweSK
, a1Gen 
, a0Gen
, eGen
, rlwePK
, shEnc
, shDec
, shAdd
, shMultC
, shMult2
, shMultF
) where 

import Data.Matrix
import qualified Data.Random.Normal as N 
import System.Random

-- polyFunc is used to add together 2 polynomials and is used in the method for polynomial multiplication

polyFunc f (x:xs) (y:ys) = [f x y] ++ polyFunc f xs ys
polyFunc f [] (y:ys) = [y] ++ polyFunc f [] ys
polyFunc f (x:xs) [] = [x] ++ polyFunc f xs []
polyFunc f [] [] = []

-- Temporary fix for polyFunc error

polyFix x y = reverse $ polyFunc (+) (reverse x) (reverse y)

-- PolyFullMult multiplies 2 polynomials together

polyFullMult (x:xs) (y:ys) = polyFix (map (*x) (y:ys)++(replicate (length xs) 0)) ([0]++polyFullMult xs (y:ys))
polyFullMult [] (y:ys) = [] -- replicate (length ys) 0   why did I have this code?
polyFullMult (x:xs) [] = []
polyFullMult [] [] = []

-- Remove the useless zeroes at the start of a polynomial

zeroRem (x:xs) 
    | x == 0 = zeroRem xs
    | otherwise = x:xs 

-- Perform division of leading terms from a polynomial

leadDiv r d q = [(head r)*(modInv (head d) q)] ++ take ((length r)-(length d))(repeat 0)

-- Add together lists of polynomials

tupAdds x y = polyFunc polyFix x y 
    
-- Reduction in a polynomial ring

polyMod n d p
    | (length n) /= 0 && (length n) >= (length d) = tupAdds [t, []] (polyMod (zeroRem $ polyFunc (+) n (map (*(-1)) (polyFullMult t d))) d p) 
    | otherwise = [[],n]
    where t = leadDiv n d p 

-- Extended Euclidean algorithm 

gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = quotRem a b
                 (s, t, g) = gcdExt b r 
             in (t, s - q * t, g)
             
-- Given a and m, return x such that ax = 1 mod m 

modInv a m = let (i, _, g) = gcdExt a m 
             in mkPos i 
             where mkPos x = if x < 0 then x + m else x 

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

-- Method to collect the Key generator elements

rlweKeyGen n q sig t = [n] ++ [q] ++ [sig] ++ [t] 

-- quick way to turn elements from a multivariate Gaussian into a discrete Gaussian
          
discreting x = map (round) x   

-- Generate the secret key using discrete Gaussian
          
rlweSK r n 
    | n <= 0 = [] 
    | otherwise = fst (N.normal (mkStdGen r)):(rlweSK (r+1) (n-1))
    
-- Generate a polynomial of degree n-1 where each coefficient is modulo q+1
    
a1Gen r q n = take n $ randomRs (0,q) (mkStdGen r)

-- public key generator

a0Gen a1 r2 r3 q n t = map (*(-1)) (polyFunc (+) (polyFullMult a1 (rlweSK r2 n)) (map (*t) (eGen r3 n)))

-- error generator for the use in the public

eGen r n
    | n <= 0 = [] 
    | otherwise = fst (N.normal (mkStdGen r)):(rlweSK (r+1) (n-1))
    
-- Collecting public and private key terms 
       
rlwePK r1 r2 r3 q n t = [a0,a1]
    where a1 = a1Gen r1 q n 
          a0 = a0Gen a1 r2 r3 q n t

-- Encryption method
          
shEnc a0 a1 t m rList n = [polyFix (polyFix (polyFullMult a0 u) (map (*t) g)) m, polyFix (polyFullMult a1 u) (map (*t) f)]
    where u = eGen (head rList) n
          g = eGen (head $ drop 1 rList) n
          f = eGen (head $ drop 2 rList) n 

-- Decryption method 

shDec s [] = []
shDec s [x] = x
shDec s (x:xs) = polyFix x (polyFullMult s (shDec s xs))

-- Addition method 

shAdd c1 c2 d p = polyMod (tupAdds c1 c2) d p

-- Multiplication component method 

shMultC c1 c2 
    | c2 == [] = []
    | otherwise = [polyFullMult c1 (head c2)] ++ shMultC c1 (tail c2)

-- Second stage of multiplication method
    
shMult2 c1 c2 
    | c1 == [] = []
    | otherwise = [shMultC (head c1) c2] ++ shMult2 (tail c1) ([[0]] ++ c2) 
  
-- Final part of multiplication method

shMultF c1 c2 = polyFunc polyFix (head term) (last term)
    where term = shMult2 c1 c2
