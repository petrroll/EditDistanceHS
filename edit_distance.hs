module EditDistance where
   import Data.Array
   
   subArrayToList :: (Ix a) => Array a b -> (a, a) -> [b]
   subArrayToList ar bnds = [ar ! i | i <- range bnds]
   
   mkArray :: (Ix a) => (a -> b) -> (a,a) -> Array a b
   mkArray f bnds = array bnds [(i, f i) | i <- range bnds]
     
   data Infinitable a = Regular a | NegativeInfinity | PositiveInfinity deriving (Eq, Show)

   instance Ord a => Ord (Infinitable a) where
      compare NegativeInfinity NegativeInfinity = EQ
      compare PositiveInfinity PositiveInfinity = EQ
      compare NegativeInfinity _ = LT
      compare PositiveInfinity _ = GT
      compare _ PositiveInfinity = LT
      compare _ NegativeInfinity = GT
      compare (Regular x) (Regular y) = compare x y    
   
   instance Num a => Num (Infinitable a) where
      negate NegativeInfinity = PositiveInfinity
      negate PositiveInfinity = NegativeInfinity
      negate (Regular a) = Regular (negate a)
      
      (+) PositiveInfinity _ = PositiveInfinity
      (+) _ PositiveInfinity = PositiveInfinity
      (+) NegativeInfinity _ = NegativeInfinity
      (+) _ NegativeInfinity = NegativeInfinity
      (+) (Regular a) (Regular b) = Regular (a + b)
      
      (*) PositiveInfinity _ = error "Unimplemented"
      (*) _ PositiveInfinity = error "Unimplemented"
      (*) NegativeInfinity _ = error "Unimplemented"
      (*) _ NegativeInfinity = error "Unimplemented"
      (*) (Regular a) (Regular b) = Regular (a * b)
      
      fromInteger a = Regular (fromInteger a)
      
      abs NegativeInfinity = PositiveInfinity
      abs PositiveInfinity = PositiveInfinity
      abs (Regular a) = Regular (abs a)
      
      signum NegativeInfinity = -1
      signum PositiveInfinity = 1
      signum (Regular a) = Regular (signum a)
   
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
       

      
   
   
