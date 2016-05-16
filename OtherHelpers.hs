module OtherHelpers (subArrayToList, emptyArray, mkArray, applyOnSuffixes, listToArray, testIfTrue) where     
   import Data.Array
   
   --Creates a list containing all elements of a subarray specified by bounds
   subArrayToList :: (Ix a) => Array a b -> (a, a) -> [b]
   subArrayToList ar bnds = [ar ! i | i <- range bnds]
   
   --Creates an empty array of wanted type
   emptyArray :: Num a => (Ix a) => Array a b
   emptyArray = array (1, 0) []
   
   --Creates an array out of bounds and a function that transforms indexes into elements
   mkArray :: (Ix a) => (a -> b) -> (a,a) -> Array a b
   mkArray f bnds = array bnds [(i, f i) | i <- range bnds]
   
   --Map on all suffixes of given list
   applyOnSuffixes :: ([a] -> b) -> [a] -> [b]
   applyOnSuffixes _ []            = []
   applyOnSuffixes fun list@(x:xs) = fun list : applyOnSuffixes fun xs
   
   --Transforms a list into an Int-index based array
   listToArray :: [a] -> Array Int a
   listToArray list = array (0, length list - 1) (zip [0..] list)
   
   --Helper method that returns one-based index of the first not-true element in an array or zero if all are true                           
   testIfTrue :: [Bool] -> Int
   testIfTrue = testIfTrueSup 1
    
   testIfTrueSup :: Int -> [Bool] -> Int
   testIfTrueSup _ [] = 0
   testIfTrueSup index (x:xs) | x         = testIfTrueSup (index + 1) xs
                              | otherwise = index