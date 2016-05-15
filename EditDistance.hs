module EditDistance where
   import Data.Array
   import InfinitySupport
   import OtherHelpers
   
   type DistScore = (Infinitable Int)
   type MemtableIndex = (Int, Int)
   type Memtable = Array MemtableIndex DistScore

   
   editDistance :: Eq a => [a] -> [a] -> DistScore
   editDistance xs ys = table ! (0, 0) where
      x = listToArray xs
      y = listToArray ys
       
      (xl, yl) = (length xs, length ys)      
      ar_bounds   = ((0, 0), (xl, yl))
         
      table = mkArray dist ar_bounds    
      frem = FUn 5 (\x -> Regular 1)
      fadd = FUn 2 (\x -> Regular 1)
      
      dist::MemtableIndex->DistScore
      dist (i, j) 
         | i > xl || j > yl   = PositiveInfinity
         | i == xl && j == yl = Regular 0
         | otherwise = minimum ([           
            if j /= yl && i /= xl
              then if (x ! i) == (y ! j)  
                then table ! (i + 1, j + 1)
                else (table ! (i + 1, j + 1)) + 1
              else PositiveInfinity
            ] ++ delOptions (i, j) 
              ++ addOptions (i, j))
          where 
            delOptions = unModOptions table Del frem x
            addOptions = unModOptions table Add fadd y
               

   data FUn a = FUn Int ([a] -> DistScore)
   data FBin a = FBin Int ([a] -> [a] -> DistScore)   
   data UnOp = Del | Add deriving (Eq)
   
   unModOptions :: Memtable -> UnOp -> FUn a -> Array Int a -> MemtableIndex -> [DistScore]
   unModOptions ar dir (FUn maxlf fmod) str (i, j) = [ (ar ! (getIndex (i, j) x)) + fmod (take x maxSubstring) | x <- [0..maxIModified]]
     where 
       lastModified = minimum [length str - 1, currPos + maxlf - 1]
       maxIModified = lastModified - currPos;
       maxSubstring = subArrayToList str (currPos, lastModified)
       currPos | dir == Del = i
               | otherwise  = j
       getIndex (i, j) x | dir == Del = (i + x + 1, j)
                         | otherwise  = (i, j + x + 1)                                               
       