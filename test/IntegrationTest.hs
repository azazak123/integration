module Main (main) where

import Math.Integration
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main =
  if (==) 0 . roundToDigit 7 . sum . fmap sum . zipWith (fmap . (-)) answers $ tests
    then exitSuccess
    else exitFailure

tests =
  [ [ integrateLeftRectangle h (const 1) nodes,
      integrateMiddleRectangle h (const 1) nodes,
      integrateRightRectangle h (const 1) nodes,
      integrateTrapezium h (const 1) nodes,
      integrateSimpson h (const 1) nodes,
      integrateGauss n (const 1) nodes
    ],
    [ integrateTrapezium h id nodes,
      integrateSimpson h id nodes,
      integrateGauss n id nodes
    ],
    [ integrateSimpson h (** 3) nodes,
      integrateGauss n (** 3) nodes
    ],
    [ integrateGauss n (** 9) nodes
    ]
  ]

a :: Double
a = 1.2

b :: Double
b = 7.5

h :: Double
h = 1.575

n :: Int
n = 4

nodes :: [Double]
nodes = [1.2, 2.775, 4.35, 5.925, 7.5]

answers :: [Double]
answers =
  [ 6.3,
    27.405,
    790.497225,
    56313514.090299
  ]

roundToDigit :: (Integral b, RealFrac a) => b -> a -> a
roundToDigit n number = (/ 10 ^ n) . fromInteger . round $ 10 ^ n * number
