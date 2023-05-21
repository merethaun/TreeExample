module FreeGen where

import Test.QuickCheck (Gen)
import qualified Test.QuickCheck.Gen as Gen

import Text.Parsec.String (Parser)
import Text.Parsec
import Data.List (find)

import Control.Monad (ap)

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

--- making it monadic
instance Functor FreeGen where
  fmap f (Return a) = Return (f a)
  fmap f (Bind m g) = Bind m (fmap f . g)
  fmap f (Pick xs) = Pick [(w, c, fmap f x) | (w,c,x) <- xs]

instance Applicative FreeGen where
  pure = Return
  (<*>) = ap

instance Monad FreeGen where
  return :: a -> FreeGen a
  return = Return

  (>>=) :: FreeGen a -> (a -> FreeGen b) -> FreeGen b
  Return a >>= f = f a
  Bind p g >>= f = Bind p (\a -> g a >>= f)

isVoid :: FreeGen a -> Bool
isVoid (Bind (Pick []) _) = True
isVoid _ = False

pick :: [(Weight, Choice, FreeGen a)] -> FreeGen a
pick xs = 
  case filter (\(_,_,x) -> not (isVoid x)) xs of
    ys | hasDuplicates (map (\(_,y,_) -> y) ys) -> undefined
    [] -> undefined
    ys -> Bind (Pick ys) Return

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs


-- Interpretations of FreeGen

--- Random generator interpretation
interpretAsG :: FreeGen a -> Gen a
interpretAsG (Return v) = return v
interpretAsG (Bind (Pick xs) f) = do
  x <- Gen.frequency (map (\(Weight w, _, x) -> ((fromIntegral w), return x)) xs)
  a <- interpretAsG x
  interpretAsG (f a)

--- Parser interpretation
interpretAsP :: FreeGen a -> Parser a
interpretAsP (Return v) = return v
interpretAsP (Bind (Pick xs) f) = do
  c <- oneOf (map (\(_, c, _) -> c) xs)
  x <- case find (\(_, c', _) -> c==c') xs of
    Just (_, _, x) -> return x
    Nothing -> fail "" -- will never occur since oneOf checks that c is a valid option
  a <- interpretAsP x
  interpretAsP (f a)

--- Randomness interpretation
interpretAsR :: FreeGen a -> Gen String
interpretAsR (Return _) = return "" -- the empty word
interpretAsR (Bind (Pick xs) f) = do
  (c, x) <- Gen.frequency (map (\(Weight w, c, x) -> ((fromIntegral w), return (c, x))) xs)
  s <- interpretAsR (x >>= f)
  return (c : s)
