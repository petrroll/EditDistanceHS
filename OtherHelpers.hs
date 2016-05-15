module OtherHelpers where     
   import Data.Array
   
   subArrayToList :: (Ix a) => Array a b -> (a, a) -> [b]
   subArrayToList ar bnds = [ar ! i | i <- range bnds]
   
   mkArray :: (Ix a) => (a -> b) -> (a,a) -> Array a b
   mkArray f bnds = array bnds [(i, f i) | i <- range bnds]