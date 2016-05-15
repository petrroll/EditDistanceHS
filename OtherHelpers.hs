module OtherHelpers where     
   import Data.Array
   
   subArrayToList :: (Ix a) => Array a b -> (a, a) -> [b]
   subArrayToList ar bnds = [ar ! i | i <- range bnds]
   
   mkArray :: (Ix a) => (a -> b) -> (a,a) -> Array a b
   mkArray f bnds = array bnds [(i, f i) | i <- range bnds]
   
   applyOnSuffixes :: ([a] -> b) -> [a] -> [b]
   applyOnSuffixes _ []            = []
   applyOnSuffixes fun list@(x:xs) = fun list : applyOnSuffixes fun xs
   
   listToArray :: [a] -> Array Int a
   listToArray list = array (0, length list - 1) (zip [0..] list)
   