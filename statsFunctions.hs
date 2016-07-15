module statsFunctions 
(
meanSetup
, meanVal
, stDevHelp
, stDevSetup
, stDevVal
--, logRegSetup
--, logRegVal
) where

import SHE 

-- Find the tuple of the encrypted values used for calculating the mean 

meanSetup :: Integral a => [[[a]]] -> [a] -> a -> ([[a]], Int)
meanSetup values d p = (total, size)
    where total = shAdd values d p
          size = length values

-- Calculate the decrypted mean 

meanVal :: Fractional b => [b] -> b -> [b]
meanVal total size = map (/size) total 

-- Standard deviation helper, calculates a single (xi - xmean)^2 value

stDevHelp :: (Num a, Eq a) => [[[a]]] -> [[[a]]]
stDevHelp values = map shSquare $ map (tupAdds $ map (map (*(-1)) ) mean) values
    where mean = head values -- temporary method to see if it works, replace with actual mean 

-- Find the tuple of the encrypted values used for calculating the standard deviation

stDevSetup :: Integral a => [[[a]]] -> [a] -> a -> ([[a]], Int)
stDevSetup values d p = (squareSum, size)
    where size = length values 
          squareSum = foldl (\acc x -> shSAdd acc x d p) [[0]] values
          
-- Calculate the decrypted standard deviation 

stDevVal :: Floating b => [b] -> b -> [b]
stDevVal squareSum size = map (sqrt) $ map (/size) squareSum