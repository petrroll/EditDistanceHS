module InfinitySupport where  
    
   data Infinitable a = Regular a | PositiveInfinity deriving (Eq, Show)

   infinitableToMaybe :: Infinitable a -> Maybe a
   infinitableToMaybe (Regular a) = Just a
   infinitableToMaybe PositiveInfinity = Nothing

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