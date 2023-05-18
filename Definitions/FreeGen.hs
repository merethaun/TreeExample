module FreeGen where

import Test.QuickCheck (Gen)
import qualified Test.QuickCheck.Gen as Gen

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

--- Random generator interpretation
interpretAsG :: FreeGen a -> Gen a
interpretAsG (Return v) = return v
interpretAsG (Bind (Pick xs) f) = do
  x <- Gen.frequency (map (\(Weight w,_,x) -> ((fromIntegral w), return x)) xs)
  a <- interpretAsG x
  interpretAsG (f a)
-- The next statemant (Bind without Pick) will most likely not be used, but needs to be defined to check all definitions of Bind
interpretAsG (Bind m f) = do
  a <- interpretAsG m
  interpretAsG (f a)

