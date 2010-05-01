module Utils
where

import Data.Foldable
import System.Random

clamp mn mx v = min mx $ max mn v

radToDeg :: (Floating a) => a -> a
radToDeg = (/pi) . (*180)

wrap :: (Num a, Ord a) => a -> a -> a -> a
wrap mn mx v =
  if v < mn
    then wrap mn mx (v + diff)
    else if v > mx
           then wrap mn mx (v - diff)
           else v
    where diff = mx - mn

safeRead :: (Read a) => String -> Maybe a
safeRead s = case reads s of
              [(n, _)] -> Just n
              _        -> Nothing

chooseIO :: (Foldable f) => f a -> IO a
chooseIO l = do
  let l' = toList l
      n = length l'
  i <- randomRIO (0, n - 1)
  return (l' !! i)

