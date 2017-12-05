module Main where

import Types
import Solution

import Control.Monad
import Data.List

{-
:: f a -> f (a -> b) -> f b
:: m a -> (a -> m b) -> m b
-}

(<**>) :: Monad m => m (a -> b) -> m a -> m b
(<**>) mf ma = mf >>= (\f -> ma >>= \a -> return $ f a)

type OrError = Either String

funny :: Int -> OrError Double
funny x = if x == 0
          then Left "Can't divide by zero"
          else Right (1 / fromIntegral x)
funny2 :: Double -> OrError Double
funny2 x = if x > 0 then Right $ sqrt x else Left "Sqrt from negative number"
funny3 :: Double -> OrError String
funny3 = Right . show

f x = do
  y1 <- funny x
  y2 <- funny2 y1
  funny3 y2

f2 x = funny x >>= (\y1 -> funny2 y1 >>= (\y2 -> funny3 y2))

g listOfLists = do
  petrov <- lookup "Petrov" listOfLists
  let abc = length petrov
  good <- find (>abc) petrov
  return good

twice :: Monad m => m a -> m a
twice act = act >> act

main :: IO ()
main = do
  putStr "x" >> putStrLn "y"
  putStrLn "hello world"

