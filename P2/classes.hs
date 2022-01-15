class Num a where 
    (+) :: a -> a ->a
    (*) :: a -> a ->a

instance Num Int where
  (+) = primPlusInt
  (*) = primMulInt