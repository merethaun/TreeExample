module FreeGen where

import Test.QuickCheck (Gen)
import qualified Test.QuickCheck.Gen as Gen

import Text.Parsec.String (Parser)
import Text.Parsec
import Data.List (find)

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
  x <- Gen.frequency (map (\(Weight w, _, x) -> ((fromIntegral w), return x)) xs)
  a <- interpretAsG x
  interpretAsG (f a)
interpretAsG (Bind m f) = do
  a <- interpretAsG m
  interpretAsG (f a) 

--- Parser interpretation
interpretAsP :: FreeGen a -> Parser a
interpretAsP (Return v) = return v
interpretAsP (Bind (Pick xs) f) = do
  c <- oneOf (map (\(_, c, _) -> c) xs)
  x <- case find (\(_, c', _) -> c==c') xs of
    Just (_, _, x) -> return x
    Nothing -> fail ""
  a <- interpretAsP x
  interpretAsP (f a)
interpretAsP (Bind m f) = do
  a <- interpretAsP m
  interpretAsP (f a)

--- Randomness interpretation
interpretAsR :: FreeGen a -> Gen String
interpretAsR (Return _) = return "" -- the empty word
interpretAsR (Bind (Pick xs) f) = do
  (c, x) <- Gen.frequency (map (\(Weight w, c, x) -> ((fromIntegral w), return (c, x))) xs)
  s <- interpretAsR (Bind x f)
  return (c : s)
interpretAsR (Bind m f) = do
  s <- interpretAsR m
  interpretAsP (f s)
