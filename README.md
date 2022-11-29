# Integration

Integration methods implemented on Haskell.

## Basic usage

Library provides different methods of integral calculation:

- Left/Middle/Right rectangles
- Trapezium
- Simpson
- Gauss

``` haskell
-- rectangles

-- left
integrateLeftRectangle 1 (**9) [1,2,3,4,5]

-- middle
integrateMiddleRectangle 1 (**9) [1,2,3,4,5] 

-- right
integrateRightRectangle 1 (**9) [1,2,3,4,5] 


-- trapezium
integrateTrapezium 1 (**9) [1,2,3,4,5] 

-- Simpson 
integrateSimpson 1 (**9) [1,2,3,4,5] 

-- Gauss
integrateGauss 4 (**9) [1,2,3,4,5]

```

## Documentation 

To generate documentation run

``` bash
cabal haddock
```

## Test

To run test run

```bash
cabal test
```
