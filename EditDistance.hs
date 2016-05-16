module EditDistance where
   import Data.Array
   import InfinitySupport
   import OtherHelpers
   
   --Helper type aliases
   type DistScore = (Infinitable Int)
   type MemtableIndex = (Int, Int)
   type Memtable = Array MemtableIndex DistScore
   
   --Data types used for definition of custom remove, add, and modify scoring functions 
   data FUn a = FUn Int ([a] -> Maybe Int) 
   data FBin a = FBin (Int, Int) ([a] -> [a] -> Maybe Int)   
   data UnOp = Del | Add deriving (Eq)
   
   --Default edit distance function that uses regular scoring functions 
   editDistanceDef :: Eq a => [a] -> [a] -> Maybe Int
   editDistanceDef = editDistance frem fadd fbin
    where 
      frem = FUn 1 (\x -> Just 1)
      fadd = FUn 1 (\x -> Just 1)
      fbin = FBin (1,1) (\x y -> if x == y then Just 0 else Just 1)
   
   --Generic edit distance function that accepts definition of custom remove, add, and modify scoring functions
   editDistance :: Eq a => FUn a -> FUn a -> FBin a -> [a] -> [a] -> Maybe Int
   editDistance frem fadd fbin xs ys = infinitableToMaybe $ table ! (0, 0) where
      
      --Transforms string list into an array for performance reasons
      x = listToArray xs 
      y = listToArray ys
       
      --Sets up bounds for memoization table and creates it
      (xl, yl) = (length xs, length ys)      
      ar_bounds   = ((0, 0), (xl, yl))
      table = mkArray dist ar_bounds
      
      --Defines distance function
      dist::MemtableIndex->DistScore
      dist (i, j) 
         | i > xl || j > yl   = PositiveInfinity --If we're out of bounds then the length is PositiveInfinity
         | i == xl && j == yl = Regular 0        --If we're at the right down corner then we're done and result is zero
         | otherwise = minimum (  --Otherwise let's take a minimum out of all the options for deletion, removal, and modification starting at current index
              delOptions (i, j) 
              ++ addOptions (i, j)
              ++ modOptions (i, j)
          )
          where 
            delOptions = unModOptions table Del frem x
            addOptions = unModOptions table Add fadd y
            modOptions = binModOptions table fbin x y               
   
   --Creates a list of all possibilities for unary modification function (removal, addition) starting at current index 
   unModOptions :: Memtable -> UnOp -> FUn a -> Array Int a -> MemtableIndex -> [DistScore]
   unModOptions ar dir (FUn maxlf fmod) str (i, j) = [ (ar ! getIndex (i, j) x) + funcValue x | x <- [1..maxModified]]
     where 
       lastModified = minimum [length str - 1, currPos + maxlf - 1]
       maxModified  = lastModified - currPos + 1;
       maxSubstring = subArrayToList str (currPos, lastModified)
       currPos | dir == Del = i
               | otherwise  = j
       getIndex (i, j) x | dir == Del = (i + x, j)
                         | otherwise  = (i, j + x)
       funcValue x = maybeToInfinity $ fmod (take x maxSubstring)
                         
   --Creates a list of all possibilities for binary modification function (modification) starting at current index                                       
   binModOptions :: Memtable -> FBin a -> Array Int a -> Array Int a -> MemtableIndex -> [DistScore]
   binModOptions ar (FBin (maxlxf, maxlyf) fmod) strX strY (i, j) = [ (ar ! (i + x, j + y)) + funcValue x y | x <- [1..maxModifiedX], y <- [1..maxModifiedY]]
     where 
       lastModified str ind maxfl = minimum [length str - 1, ind + maxfl - 1] 
       lastModifiedX = lastModified strX  i maxlxf
       lastModifiedY = lastModified strY  j maxlyf
       
       maxModifiedX  = lastModifiedX - i + 1;
       maxModifiedY  = lastModifiedY - j + 1;
       
       maxSubstringX = subArrayToList strX (i, lastModifiedX)
       maxSubstringY = subArrayToList strY (j, lastModifiedY)
       funcValue x y = maybeToInfinity $ fmod (take x maxSubstringX) (take y maxSubstringY)
       
                         
                         

                                                                                                      