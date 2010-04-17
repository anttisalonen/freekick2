module FVector
where

type FRange = (Float, Float)

type FVector3 = (Float, Float, Float)

getX, getY, getZ :: FVector3 -> Float
getX (x, _, _) = x
getY (_, y, _) = y
getZ (_, _, z) = z

addX, addY, addZ :: Float -> FVector3 -> FVector3
addX v (x, y, z) = (v + x, y, z)
addY v (x, y, z) = (x, v + y, z)
addZ v (x, y, z) = (x, y, v + z)

setX, setY, setZ :: Float -> FVector3 -> FVector3
setX v (_, y, z) = (v, y, z)
setY v (x, _, z) = (x, v, z)
setZ v (x, y, _) = (x, y, v)

(*+*) :: FVector3 -> FVector3 -> FVector3
(x1, y1, z1) *+* (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

(*-*) :: FVector3 -> FVector3 -> FVector3
(x1, y1, z1) *-* (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

(***) :: FVector3 -> Float -> FVector3
(x1, y1, z1) *** s = (x1 * s, y1 * s, z1 * s)

to2D :: FVector3 -> FRange
to2D (x, y, _) = (x, y)

nullFVector3 :: FVector3
nullFVector3 = (0, 0, 0)

dist2squared :: FRange -> FRange -> Float
dist2squared (x1, y1) (x2, y2) = ((x2 - x1)**2) + ((y2 - y1)**2)

dist2 :: FRange -> FRange -> Float
dist2 p1 p2 = sqrt $ dist2squared p1 p2

len2 :: FRange -> Float
len2 v = dist2 v (0, 0)

diff2 :: FRange -> FRange -> FRange
diff2 (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

mul2 :: FRange -> Float -> FRange
mul2 (x, y) s = (x * s, y * s)

to3D :: FRange -> Float -> FVector3
to3D (x, y) z = (x, y, z)
