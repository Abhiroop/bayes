{-# LANGUAGE TupleSections #-}
module Main where

import Control.Applicative
import Text.Printf
import qualified Data.Map as M


{-@ Probability Monad modeled after https://dennybritz.com/posts/probability-monads-from-scratch/
@-}


type Prob = Double

newtype Dist a = Dist { unpackDist :: [(a, Prob)] }

instance (Show a, Ord a) => Show (Dist a) where
  show d = concatMap showRow $ (unpackDist) d
    where
      showRow (elem, prob) =
        padded elem ++ " | " ++ printf "%.4f" prob ++ "\n"
      padded elem =
        replicate (maxElemLen - (length . show) elem) ' ' ++ show elem
      maxElemLen = maximum $ map (length . show . fst) (unpackDist d)

instance Functor Dist where
  fmap f (Dist xs) = Dist $ [(f x, p) | (x, p) <- xs]

instance Applicative Dist where
  -- pure :: a -> Dist a
  pure x = Dist [(x, 1.0)]

  -- (<*>) :: Dist (a -> b) -> Dist a -> Dist b
  (Dist fs) <*> (Dist xs) = Dist $ do
    (x, px) <- xs
    (f, pf) <- fs
    return (f x, px * pf)

instance Monad Dist where
  -- (>>=) :: Dist a -> (a -> Dist b) -> Dist b
  (Dist xs) >>= f = Dist $ do
    (x, p)  <- xs
    (y, p') <- unpackDist (f x)
    return (y, p * p')


join :: Dist (Dist a) -> Dist a
join (Dist dist) = Dist $ do -- dist  :: [(Dist a, Prob)]
  (Dist dista, prob) <- dist -- dista :: [(a, Prob)]
  (a, prob2)         <- dista
  return (a, prob * prob2)

squishD :: (Ord a) => Dist a -> Dist a
squishD = Dist . M.toList . M.fromListWith (+) . unpackDist

sumP :: [(a, Prob)] -> Prob
sumP = sum . map snd

normP :: [(a, Prob)] -> [(a, Prob)]
normP xs = map (\(x, y) -> (x, y / (sumP xs))) xs

type Event a = a -> Bool

-- | Evaluate the probability for the given event
evalD :: Event a -> Dist a -> Prob
evalD p = sumP . filter (p . fst) . unpackDist

-- | Uniform Distribution
uniform :: [a] -> Dist a
uniform = Dist . normP . map (, 1.0)

-- | A fair n-sided die
die :: Int -> Dist Int
die n = uniform [1 .. n]

-- | A coin that lands on x with probability f and y with probability 1-f
coin :: Prob -> a -> a -> Dist a
coin f x y
  | f < 0.0 || f > 1.0 = error "f must be between 0 and 1"
  | otherwise = Dist [(x, f), (y, 1 - f)]

sample =
  Dist
    [ ((0, 0), 0.1),
      ((0, 1), 0.2),
      ((1, 0), 0.3),
      ((1, 1), 0.4)
    ]

binom :: Int -> Prob -> Dist Int
binom n p =
  foldl1 (\x y -> squishD (liftA2 (+) x y)) $ replicate n (coin p 1 0)

conditionalCoin = do
  number <- die 6
  if number == 6
    then coin 0.5 1 0
    else coin 0.1 1 0

condD :: (a -> Bool) -> Dist a -> Dist a
condD f (Dist xs) = Dist . normP $ filter (f . fst) xs

bayesMedicalTest = evalD fst . condD snd $ do
  hasDisease <- coin 0.01 True False
  testPositive <-
    if hasDisease
      then coin 0.95 True False
      else coin 0.05 True False
  return (hasDisease, testPositive)


certainly :: a -> Dist a
certainly a = Dist [(a, 1.0)]

data Outcome = Win | Loss deriving (Show, Eq, Ord)

switching :: Dist Outcome
switching = do
  firstChoice <- uniform [Win,Loss,Loss]
  if (firstChoice == Win)
  then {- switching will -} certainly Loss
  else {- switching will -} certainly Win



main :: IO ()
main = putStrLn "Hello, Haskell!"
