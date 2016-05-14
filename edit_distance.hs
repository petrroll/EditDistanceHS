module EditDistance where
   import Data.Array
   
   mkArray :: (Ix a) => (a -> b) -> (a,a) -> Array a b
   mkArray f bnds = array bnds [(i, f i) | i <- range bnds]
   
   editDistance :: Eq a => [a] -> [a] -> Int
   editDistance xs ys = table ! (0, 0) where
      x        = array (0, xl) (zip [0..] xs)
      y        = array (0, yl) (zip [0..] ys)
       
      (xl, yl) = (length xs, length ys)      
      ar_bounds   = ((0, 0), (xl, yl))
         
      table = mkArray dist ar_bounds    
      
      dist::(Int, Int) -> Int
      dist (i, j) 
         | i == xl = yl - j
         | j == yl = xl - i
         | otherwise = minimum [
            (table ! (i, j + 1)) + 1,
            (table ! (i + 1, j)) + 1,
            if (x ! i) == (y ! j) then table ! (i + 1, j) + 1
              else table ! (i + 1, j + 1) + 1
            ]   
       

      
   
   
