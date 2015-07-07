
class Dionom i where
  dempty :: i
  dappend :: i -> i -> i
  dconcat :: [i] -> i
  dconcat = foldr dappend dempty

instance Dionom [a] where
  dempty = []
  dappend x y = x ++ y

newtype Sums a = Sums { getSum :: a }
instance Num a => Dionom (Sums a) where
  dempty = Sums 0
  dappend x y  = Sums $ getSum x + getSum y

newtype Products a = Products { getProduct :: a }
instance Num a => Dionom (Products a) where
  dempty = Products 1
  dappend x y  = Products $ getProduct x * getProduct y
