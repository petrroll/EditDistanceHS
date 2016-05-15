module EditDistance where
   import Data.Array
   
   subArrayToList :: (Ix a) => Array a b -> (a, a) -> [b]
   subArrayToList ar bnds = [ar ! i | i <- range bnds]
   
   mkArray :: (Ix a) => (a -> b) -> (a,a) -> Array a b
   mkArray f bnds = array bnds [(i, f i) | i <- range bnds]
   
   data Infinitable a = Regular a | PositiveInfinity deriving (Eq, Show)

   instance Ord a => Ord (Infinitable a) where
      compare PositiveInfinity PositiveInfinity = EQ
      compare PositiveInfinity _ = GT
      compare _ PositiveInfinity = LT
      compare (Regular x) (Regular y) = compare x y    
   
   instance (Eq a, Num a) => Num (Infinitable a) where
      negate PositiveInfinity = error "Unsupported operation"
      negate (Regular a) = Regular (negate a)
      
      (+) PositiveInfinity _ = PositiveInfinity
      (+) _ PositiveInfinity = PositiveInfinity
      (+) (Regular a) (Regular b) = Regular (a + b)
      
      (*) PositiveInfinity b | signum b == Regular 1 = PositiveInfinity
                             | otherwise = error "Unsupported operation"
                        
      (*) a PositiveInfinity | signum a == Regular 1 = PositiveInfinity
                             | otherwise = error "Unsupported operation"                                            
      (*) (Regular a) (Regular b) = Regular (a * b)
      
      fromInteger a = Regular (fromInteger a)

      abs PositiveInfinity = PositiveInfinity
      abs (Regular a) = Regular (abs a)

      signum PositiveInfinity = Regular 1
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
       

      
   
   
