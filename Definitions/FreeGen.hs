module FreeGen where


-- Basic definition of the type FreeGen (and other necessary definitions)
data FreeGen a where
  Return :: a -> FreeGen a
  Bind :: FreeGen a -> (a -> FreeGen b) -> FreeGen b
  Pick :: [(Weight, Choice, FreeGen a)] -> FreeGen a


newtype Weight = Weight {toInt :: Integer} deriving (Eq, Ord)

instance Num Weight where
  Weight x + Weight y = Weight (x+y)
  Weight x * Weight y = Weight (x*y)
  abs = id
  signum = id
  fromInteger i = if (i<=0) then error "invalid weight" else Weight i
  negate = error "cannot negate weight"

instance Show Weight where show = show . toInt

type Choice = Char

-- Interpretations of FreeGen

