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
      frem = FRem 5 (\x -> Regular 1)
      
      dist::(Int, Int) -> Infinitable Int
      dist (i, j) 
         | i > xl || j > yl   = PositiveInfinity
         | i == xl && j == yl = Regular 0
         | i == xl            = Regular (yl - j)
         | otherwise = minimum ([
            if j /= yl 
              then (table ! (i, j + 1)) + 1
              else PositiveInfinity,
            
            if j /= yl 
              then if (x ! i) == (y ! j)  
                then table ! (i + 1, j + 1)
                else (table ! (i + 1, j + 1)) + 1
              else PositiveInfinity
            ] ++ remOptions table frem x (i, j))
               

   data FRem a = FRem Int ([a] -> Infinitable Int)
   data FAdd a = FAdd Int ([a] -> Infinitable Int)
   data FMod a = FMod Int ([a] -> [a] -> Infinitable Int)      
   

   remOptions :: Array (Int, Int) (Infinitable Int) -> FRem a -> Array Int a -> (Int, Int) -> [Infinitable Int]
   remOptions ar (FRem maxlf frem) xs (i, j) = [ (ar ! (i + x + 1, j)) + frem (take x maxSubstring) | x <- [0..maxIDeleted]]
     where 
       lastDeleted = minimum [length xs - 1, i + maxlf - 1]
       maxIDeleted = lastDeleted - i;
       maxSubstring = subArrayToList xs (i, lastDeleted)
  