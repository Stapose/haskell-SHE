

--main = do
--    putStrLn "Give the parameters for the cryptosystem"
--    name2 <- getLine
--    putStrLn $ "The parameters you have chosen are" ++ (show $ read name2)
--    putStrLn "Pass a list of values to encrypt an analyse"
--    name <- getLine
--    putStrLn $ "The encrypted values are" ++ (show $ addOne(read name))  
    
--import System.Random
import Control.Monad(when)
import SHE 

--addOne x = map (+1) x
--import statsFunctions

--main = do 
--    gen <- getStdGen
--    askForNumber gen
    
--askForNumber :: StdGen -> IO ()
--askForNumber gen = do
--    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
--    putStrLn "Which number in the range from 1 to 10 am I thinking of? "
--    numberString <- getLine 
--    if not $ null numberString
--        then do
--            let number = read numberString
--            if randNumber == number 
--                then putStrLn "You are correct!"
--                else putStrLn $ "Sorry, it was " ++ show randNumber 
--            askForNumber newGen
--    else putStrLn "No number"
    
main = do
    putStrLn "Give the order of the polynomial to generate the secret key"
    secretOrd <- getLine
    let y = read secretOrd :: Int
    print y
    putStrLn "Set first seed"
    seed1 <- getLine
    let x = read seed1 :: Int
    print x
    putStrLn "Give public key"
    public <- getLine
    
    --putStrLn $ "the total is " ++ (show $ findTotal (read secretOrd) (read seed1) (read public))
    let z = rlweSK x y 
    putStrLn "Done"
    
    
    
    --valueToEnc secretOrd public seed1

--valueToEnc secretOrd public seed1 = do
--    putStrLn "What value do you want to encrypt"
--    message <- getLine
    --let secretOrdval = secretOrd
    --let seed1val = read seed1
    --let publicval = read public
--    putStrLn $ "Your secret key is "  ++ (show $ addOne(read seed1)) -- show seed1 -- ++ show $ rlweSK seed1 (read secretOrd)
    

    
    
    
    