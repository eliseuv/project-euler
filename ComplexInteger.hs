module ComplexInteger
  ( ComplexInteger ((:+)),
    realPart,
    imagPart,
    magnitude,
    magnitudeSq,
  )
where

data ComplexInteger a = !a :+ !a
  deriving
    ( Eq,
      Show,
      Read
    )

realPart :: ComplexInteger a -> a
realPart (x :+ _) = x

imagPart :: ComplexInteger a -> a
imagPart (_ :+ y) = y

magnitude :: (Integral a, RealFloat b) => ComplexInteger a -> b
magnitude (x :+ y) = sqrt . sum $ map (sqrt . fromIntegral) [x, y]

magnitudeSq :: Num a => ComplexInteger a -> a
magnitudeSq (x :+ y) = sum $ map sq [x, y]
  where
    sq z = z * z

instance (Integral a) => Num (ComplexInteger a) where
  (x :+ y) + (x' :+ y') = (x + x') :+ (y + y')
  (x :+ y) - (x' :+ y') = (x - x') :+ (y - y')
  (x :+ y) * (x' :+ y') = (x * x' - y * y') :+ (x * y' + y * x')
  negate (x :+ y) = negate x :+ negate y

  -- This one is botched!
  abs z = (floor . magnitude) z :+ 0

  -- Signum here returns the quadrant
  signum (0 :+ 0) = 0
  signum (x :+ y) = signum x :+ signum y
  fromInteger n = fromInteger n :+ 0
