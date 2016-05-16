module EditDistance where
   import Data.Array
   import InfinitySupport
   import OtherHelpers
   
   type DistScore = (Infinitable Int)
   type MemtableIndex = (Int, Int)
   type Memtable = Array MemtableIndex DistScore

    
   editDistanceDef :: Eq a => [a] -> [a] -> Maybe Int
   editDistanceDef = editDistance frem fadd fbin
    where 
      frem = FUn 1 (\x -> Regular 1)
      fadd = FUn 1 (\x -> Regular 1)
      fbin = FBin (1,1) (\x y -> if x == y then Regular 0 else Regular 1)
   
   editDistance :: Eq a => FUn a -> FUn a -> FBin a -> [a] -> [a] -> Maybe Int
   editDistance frem fadd fbin xs ys = infinitableToMaybe $ table ! (0, 0) where
      x = listToArray xs
      y = listToArray ys
       
      (xl, yl) = (length xs, length ys)      
      ar_bounds   = ((0, 0), (xl, yl))
         
      table = mkArray dist ar_bounds
      
      dist::MemtableIndex->DistScore
      dist (i, j) 
         | i > xl || j > yl   = PositiveInfinity
         | i == xl && j == yl = Regular 0
         | otherwise = minimum (
              delOptions (i, j) 
              ++ addOptions (i, j)
              ++ modOptions (i, j)
          )
          where 
            delOptions = unModOptions table Del frem x
            addOptions = unModOptions table Add fadd y
            modOptions = binModOptions table fbin x y 
               

   data FUn a = FUn Int ([a] -> DistScore)
   data FBin a = FBin (Int, Int) ([a] -> [a] -> DistScore)   
   data UnOp = Del | Add deriving (Eq)
   
   unModOptions :: Memtable -> UnOp -> FUn a -> Array Int a -> MemtableIndex -> [DistScore]
   unModOptions ar dir (FUn maxlf fmod) str (i, j) = [ (ar ! getIndex (i, j) x) + fmod (take x maxSubstring) | x <- [1..maxModified]]
     where 
       lastModified = minimum [length str - 1, currPos + maxlf - 1]
       maxModified  = lastModified - currPos + 1;
       maxSubstring = subArrayToList str (currPos, lastModified)
       currPos | dir == Del = i
               | otherwise  = j
       getIndex (i, j) x | dir == Del = (i + x, j)
                         | otherwise  = (i, j + x)
                         
                                         
   binModOptions :: Memtable -> FBin a -> Array Int a -> Array Int a -> MemtableIndex -> [DistScore]
   binModOptions ar (FBin (maxlxf, maxlyf) fmod) strX strY (i, j) = [ (ar ! (i + x, j + y)) + funcMod x y | x <- [1..maxModifiedX], y <- [1..maxModifiedY]]
     where 
       lastModified str ind maxfl = minimum [length str - 1, ind + maxfl - 1] 
       lastModifiedX = lastModified strX  i maxlxf
       lastModifiedY = lastModified strY  j maxlyf
       
       maxModifiedX  = lastModifiedX - i + 1;
       maxModifiedY  = lastModifiedY - j + 1;
       
       maxSubstringX = subArrayToList strX (i, lastModifiedX)
       maxSubstringY = subArrayToList strY (j, lastModifiedY)
       funcMod x y = fmod (take x maxSubstringX) (take y maxSubstringY)
       
                         
                         

                                                                                                      