module Tests where
    import EditDistance
    import InfinitySupport
    
    type EDSetup a = [a] -> [a] -> Maybe Int 

    simpleEDSetup :: Eq a => [a] -> [a] -> Maybe Int                          
    simpleEDSetup = editDistanceDef
    advEDSEtup :: [Char] -> [Char] -> Maybe Int 
    advEDSEtup = editDistance frem fadd fbin
        where 
           frem = FUn 3 (\x -> if x == [head x | a <- [1..(length x)]] then Regular 1 else Regular (length x))
           fadd = FUn 3 (\x -> if x == [head x | a <- [1..(length x)]] then Regular 1 else Regular (length x))
           fbin = FBin (4,4) (\x y -> if x == y then Regular 0 else (if x == "abcd" && y == "cdfe" then Regular 1 else Regular (length x + length y)))


    testStrings :: (Eq a) => EDSetup a -> [a] -> [a] -> Maybe Int -> Bool
    testStrings edMachine x y expResult | expResult == result = True
                                        | otherwise           = False
                                       where
                                          result = edMachine x y
                                 
    testIfTrue :: [Bool] -> Int
    testIfTrue = testIfTrueSup 1
    
    testIfTrueSup :: Int -> [Bool] -> Int
    testIfTrueSup _ [] = 0
    testIfTrueSup index (x:xs) | x      = testIfTrueSup (index + 1) xs
                            | otherwise = index
   
    simpleTests :: Int                          
    simpleTests = 
        testIfTrue [
            testFunc "a" "a" (Just 0),
            testFunc "ab" "a" (Just 1),
            testFunc "a" "ab" (Just 1),
            testFunc "sunday" "saturday" (Just 3)
         ] 
       where 
         testFunc = testStrings simpleEDSetup
         
    advTests :: Int                          
    advTests = 
        testIfTrue [
            testFunc "abb" "a" (Just 1),
            testFunc "abc" "a" (Just 2),
            testFunc "a" "abb" (Just 1),
            testFunc "a" "abc" (Just 2),
            testFunc "abbb" "accc" (Just 2),
            testFunc "aabcdd" "acdfed" (Just 1),
            testFunc "alabcdd" "acdfepd" (Just 3) 
         ] 
       where 
         testFunc = testStrings advEDSEtup