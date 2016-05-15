module EditDistance where
   import Data.Array
   import InfinitySupport
   import OtherHelpers
   
   editDistance :: Eq a => [a] -> [a] -> Infinitable Int
   editDistance xs ys = table ! (0, 0) where
      x = listToArray xs
      y = listToArray ys
       
      (xl, yl) = (length xs, length ys)      
      ar_bounds   = ((0, 0), (xl, yl))
         
      table = mkArray dist ar_bounds    
      
      dist::(Int, Int) -> Infinitable Int
      dist (i, j) 
         | i > xl || j > yl = PositiveInfinity
         | i == xl          = Regular (yl - j)
         | j == yl          = Regular (xl - i)
         | otherwise = minimum [
            (table ! (i, j + 1)) + 1,
            (table ! (i + 1, j)) + 1,
            if (x ! i) == (y ! j) then table ! (i + 1, j + 1)
              else (table ! (i + 1, j + 1)) + 1
            ]
               
   
   data FRem a b = FRem Int ([a] -> b)
   data FAdd a b = FAdd Int ([a] -> b)
   data FMod a b = FMod Int ([a] -> [a] -> b)      
  