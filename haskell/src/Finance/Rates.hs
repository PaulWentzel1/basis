-- | Interest-rate curve construction and interpolation.
module Finance.Rates
    ( Time
    , mkTime
    , unTime
    , Rate
    , mkRate
    , unRate
    , Node
    , InterpolationMethod (..)
    , Curve
    , curveNodes
    , curveMethod
    , mkCurve
    , rate
    ) where

import Data.List.NonEmpty (NonEmpty (..))

-- | A point in time, in years from the curve's valuation date.
newtype Time = Time { unTime :: Double }
    deriving (Show, Eq, Ord)

-- | Interest rate expressed as decimal (e.g. 0.03 for 3%).
newtype Rate = Rate { unRate :: Double }
    deriving (Show, Eq, Ord)

mkTime :: Double -> Time
mkTime = Time

mkRate :: Double -> Rate
mkRate = Rate

-- | A single (time, rate) point on a curve.
type Node = (Time, Rate)

data InterpolationMethod
    = Linear
    | LogLinear
    | Akima
    | CubicSpline
    | MonotoneConvex
    deriving (Show, Eq)

-- | A yield curve: a non-empty, time-ordered set of nodes plus the interpolation method used to evaluate rates between the given ones.
data Curve = Curve
    { curveNodes  :: NonEmpty Node
    , curveMethod :: InterpolationMethod
    } deriving (Show)

-- | Smart constructor. A single-node curve is always treated as flat, anchored at t = 0, regardless of the requested method
mkCurve :: InterpolationMethod -> NonEmpty Node -> Curve
mkCurve _      ((_, r) :| []) = Curve ((mkTime 0, r) :| []) Linear
mkCurve method nodes          = Curve nodes method

interpolateFor :: InterpolationMethod -> NonEmpty Node -> Time -> Rate
interpolateFor Linear         = linearInterp
interpolateFor LogLinear      = logLinearInterp
interpolateFor CubicSpline    = cubicSplineInterp
interpolateFor Akima          = akimaInterp
interpolateFor MonotoneConvex = monotoneConvexInterp

rate :: Curve -> Time -> Rate
rate (Curve nodes method) = interpolateFor method nodes

linearInterp :: NonEmpty Node -> Time -> Rate
linearInterp _ _ = undefined

logLinearInterp :: NonEmpty Node -> Time -> Rate
logLinearInterp _ _ = undefined

cubicSplineInterp :: NonEmpty Node -> Time -> Rate
cubicSplineInterp _ _ = undefined

akimaInterp :: NonEmpty Node -> Time -> Rate
akimaInterp _ _ = undefined

monotoneConvexInterp :: NonEmpty Node -> Time -> Rate
monotoneConvexInterp _ _ = undefined


exampleFlatCurve :: Curve
exampleFlatCurve = mkCurve Linear singleNode
  where
    singleNode :: NonEmpty Node
    singleNode = (mkTime 5, mkRate 0.02) :| []

