module EditDistance where
   import Data.Array
   import InfinitySupport
   
   subArrayToList :: (Ix a) => Array a b -> (a, a) -> [b]
   subArrayToList ar bnds = [ar ! i | i <- range bnds]
   
   mkArray :: (Ix a) => (a -> b) -> (a,a) -> Array a b
   mkArray f bnds = array bnds [(i, f i) | i <- range bnds]
   
   editDistance :: Eq a => [a] -> [a] -> Infinitable Int
   editDistance xs ys = table ! (0, 0) where
      x        = array (0, xl) (zip [0..] xs)
      y        = array (0, yl) (zip [0..] ys)
       
      (xl, yl) = (length xs, length ys)      
      ar_bounds   = ((0, 0), (xl, yl))
         
      table = mkArray dist ar_bounds    
      
      dist::(Int, Int) -> Infinitable Int
      dist (i, j) 
         | i == xl = Regular (yl - j)
         | j == yl = Regular (xl - i)
         | otherwise = minimum [
            (table ! (i, j + 1)) + 1,
            (table ! (i + 1, j)) + 1,
            if (x ! i) == (y ! j) then table ! (i + 1, j + 1)
              else (table ! (i + 1, j + 1)) + 1
            ]   
       

      
   
   
