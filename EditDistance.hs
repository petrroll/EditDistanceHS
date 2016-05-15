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
      frem = FUn 5 (\x -> Regular 1)
      fadd = FUn 2 (\x -> Regular 1)
      
      dist::(Int, Int) -> Infinitable Int
      dist (i, j) 
         | i > xl || j > yl   = PositiveInfinity
         | i == xl && j == yl = Regular 0
         | otherwise = minimum ([           
            if j /= yl && i /= xl
              then if (x ! i) == (y ! j)  
                then table ! (i + 1, j + 1)
                else (table ! (i + 1, j + 1)) + 1
              else PositiveInfinity
            ] ++ modOptions table Horizontal frem x (i, j) 
              ++ modOptions table Vertical fadd y (i, j))
               

   data FUn a = FUn Int ([a] -> Infinitable Int)
   data FBin a = FBin Int ([a] -> [a] -> Infinitable Int)   
   data Direction = Horizontal | Vertical deriving (Eq)
   

   modOptions :: Array (Int, Int) (Infinitable Int) -> Direction -> FUn a -> Array Int a -> (Int, Int) -> [Infinitable Int]
   modOptions ar dir (FUn maxlf fmod) str (i, j) = [ (ar ! (getIndex (i, j) x)) + fmod (take x maxSubstring) | x <- [0..maxIModified]]
     where 
       lastModified = minimum [length str - 1, currPos + maxlf - 1]
       maxIModified = lastModified - currPos;
       maxSubstring = subArrayToList str (currPos, lastModified)
       currPos | dir == Horizontal = i
               | otherwise         = j
       getIndex (i, j) x | dir == Horizontal = (i + x + 1, j)
                         | otherwise         = (i, j + x + 1)
       