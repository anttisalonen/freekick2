module Utils
where

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

