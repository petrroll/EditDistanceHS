module Tests (simpleTests, advTests) where
   import EditDistance
   import InfinitySupport
   import OtherHelpers
   
   type EDSetup a = [a] -> [a] -> Maybe Int 
   
   --Prepares editDistance functions for simple (regular score functions) and advanced (my own weird score functions) tests                  
   simpleEDSetup = editDistanceDef
   advEDSetup = editDistance frem fadd fbin
      where 
         frem = FUn 3 (\x -> if x == [head x | a <- [1..(length x)]] then Just 1 else Just (length x))
         fadd = FUn 3 (\x -> if x == [head x | a <- [1..(length x)]] then Just 1 else Just (length x))
         fbin = FBin (4,4) (\x y -> if x == y then Just 0 else (if x == "abcd" && y == "cdfe" then Just 1 else Just (length x + length y)))
   --Tests editDistance setup with two inputs and expected output and returns result of such test (pass / fail)
   testStrings :: (Eq a) => EDSetup a -> [a] -> [a] -> Maybe Int -> Bool
   testStrings edFunction x y expResult | expResult == result = True
                                        | otherwise           = False
                                      where
                                        result = edFunction x y
                                         
  
   --Simple tests that use traditional edit distance scoring functions
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
   
   --Advanced tests that use very strange scoring funcions for edit, remove, and modify operations.
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
        testFunc = testStrings advEDSetup