module Math.Integration where

import Math.Integration.Coeficients

-- | Integrate using left rectangles formula
integrateLeftRectangle :: Num a => a -> (b -> a) -> [b] -> a
integrateLeftRectangle h f = integrateRectangle h f . init

-- | Integrate using right rectangles formula
integrateRightRectangle :: Num a => a -> (b -> a) -> [b] -> a
integrateRightRectangle h f = integrateRectangle h f . tail

-- | Integrate using middle rectangles formula
integrateMiddleRectangle :: (Num a, Fractional b) => a -> (b -> a) -> [b] -> a
integrateMiddleRectangle h f xs =
  integrateRectangle h f
    . fmap (/ 2)
    . zipWith
      (+)
      xs
    . tail
    $ xs

integrateRectangle :: (Num a, Foldable t, Functor t) => a -> (b -> a) -> t b -> a
integrateRectangle h f = (*) h . sum . fmap f

-- | Integrate using trapeziums formula
integrateTrapezium :: Fractional a => a -> (b -> a) -> [b] -> a
integrateTrapezium h f xs =
  (+) addend
    . integrateRectangle h f
    . init
    . tail
    $ xs
  where
    addend = (h / 2) * (f (head xs) + f (last xs))

-- | Integrate using Simpson formula
integrateSimpson :: Fractional a => a -> (b -> a) -> [b] -> a
integrateSimpson h f xs =
  (*) h
    . (/ 3)
    . (+) addend
    . (+) (elements 2 0)
    $ elements 4 1
  where
    elements coef eq =
      integrateRectangle coef f
        . fmap snd
        . filter ((== eq) . (`rem` 2) . fst)
        . init
        . tail
        . zip [0 :: Int ..]
        $ xs
    addend = f (head xs) + f (last xs)

-- | Integrate using Gaussian quadrature
integrateGauss :: Int -> (Double -> Double) -> [Double] -> Double
integrateGauss n f xs =
  (*) sub
    . sum
    . zipWith (*) (a !! n)
    . fmap (f . (+) add . (*) sub)
    . (!! n)
    $ t
  where
    add = (head xs + last xs) / 2
    sub = (last xs - head xs) / 2
