module D3.Axis
  (
    Axis
  , TickParams(..)
  , d3AxisBottom
  , d3AxisLeft
  , d3AxisRight
  , d3AxisTop
  , axisTicks
  , renderAxis
    )where

import D3.Base (D3, Eff)
import D3.Scale (Scale)
import D3.Selection (Selection)
import Effect.Uncurried (runEffectFn2, EffectFn3, runEffectFn3, EffectFn2, runEffectFn1, EffectFn1)
import Data.Maybe (Maybe(Just, Nothing))

foreign import data Axis :: Type -> Type

foreign import d3AxisTopFn    :: ∀ d r eff. EffectFn1 (Scale d r) (Axis d)
foreign import d3AxisBottomFn :: ∀ d r eff. EffectFn1 (Scale d r) (Axis d)
foreign import d3AxisRightFn  :: ∀ d r eff. EffectFn1 (Scale d r) (Axis d)
foreign import d3AxisLeftFn   :: ∀ d r eff. EffectFn1 (Scale d r) (Axis d)

d3AxisTop :: ∀ d r eff. Scale d r -> Effect (Axis d)
d3AxisTop = runEffectFn1 d3AxisTopFn

d3AxisBottom :: ∀ d r eff. Scale d r -> Effect (Axis d)
d3AxisBottom = runEffectFn1 d3AxisBottomFn

d3AxisRight :: ∀ d r eff. Scale d r -> Effect (Axis d)
d3AxisRight = runEffectFn1 d3AxisRightFn

d3AxisLeft :: ∀ d r eff. Scale d r -> Effect (Axis d)
d3AxisLeft = runEffectFn1 d3AxisLeftFn

foreign import d3AxisTicksCountFn     :: ∀ d eff. EffectFn2 Number (Axis d)        (Axis d)
foreign import d3AxisTicksCountSFn    :: ∀ d eff. EffectFn3 Number String (Axis d) (Axis d)
foreign import d3AxisTicksIntervalFn  :: ∀ d eff. EffectFn2 Number (Axis d)        (Axis d)
foreign import d3AxisTicksIntervalSFn :: ∀ d eff. EffectFn3 Number String (Axis d) (Axis d)

data TickParams = Count Number (Maybe String)
                 | Interval Number (Maybe String)  -- there are more possibilities here to write TODO

axisTicks :: ∀ d eff. TickParams -> Axis d -> Effect (Axis d)
axisTicks (Count n Nothing)     = runEffectFn2 d3AxisTicksCountFn n
axisTicks (Count n (Just s))    = runEffectFn3 d3AxisTicksCountSFn n s
axisTicks (Interval i Nothing)  = runEffectFn2 d3AxisTicksIntervalFn i
axisTicks (Interval i (Just s)) = runEffectFn3 d3AxisTicksIntervalSFn i s


-- | pretty sure this is unnecessary - didn't i implement all those selection
-- call functions for a reason? however, just want to get this axis working
-- right now and revisit TODO
foreign import renderAxisFn :: ∀ d a eff. EffectFn2 (Axis a) (Selection d) (Selection d)

renderAxis :: ∀ a d eff. Axis a -> Selection d -> Effect (Selection d)
renderAxis = runEffectFn2 renderAxisFn

-- # axis.ticks(arguments…) <>
-- # axis.ticks([count[, specifier]])
-- # axis.ticks([interval[, specifier]])
--
-- Sets the arguments that will be passed to scale.ticks and scale.tickFormat when the axis is rendered, and returns the axis generator. The meaning of the arguments depends on the axis’ scale type: most commonly, the arguments are a suggested count for the number of ticks (or a time interval for time scales), and an optional format specifier to customize how the tick values are formatted.
--
-- For example, to generate twenty ticks with SI-prefix formatting on a linear scale, say:
--
-- axis.ticks(20, "s");
-- To generate ticks every fifteen minutes with a time scale, say:
--
-- axis.ticks(d3.timeMinute.every(15));
-- This method is an alternative to setting the tick values explicitly via axis.tickValues, and setting the tick format explicitly via axis.tickFormat. This method is also a convenience function for axis.tickArguments. For example, this:
--
