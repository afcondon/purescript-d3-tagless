module D3.Scale
  ( Scale
  , ScaleType(..)
  , SchemeCategory
  , schemeCategory10
  , schemeCategory20
  , schemeCategory20b
  , schemeCategory20c
  , d3Scale
  , bandwidth
  , clamp
  , domain
  , interpolate
  , invert
  , nice
  , padding
  , paddingInner
  , paddingOuter
  , range
  , rangeRound
  , round
  , scaleBy
  , tickFormat
  , ticks
  ) where

import Effect
import D3.Collections
import Effect.Uncurried (runEffectFn2, runEffectFn3, runEffectFn1, EffectFn2, EffectFn3, EffectFn1)
import Data.Maybe (Maybe(..))

foreign import data Scale :: Type -> Type -> Type

type Interpolator = (Number -> String)
type Format       = String

-- | This belongs in d3-scale-chromatic here just for now, to be broken out later
foreign import data SchemeCategory :: Type       -- in practice it's just a string, i think
foreign import schemeCategory10  :: SchemeCategory
foreign import schemeCategory20  :: SchemeCategory
foreign import schemeCategory20b :: SchemeCategory
foreign import schemeCategory20c :: SchemeCategory

type D3LinearScale d r    = Scale d r
type D3LogScale d r       = Scale d r
type D3PowerScale d r     = Scale d r
type D3IdentityScale d r  = Scale d r
type D3TimeScale d r      = Scale d r
type D3BandScale d r      = Scale d r
type D3PointScale d r     = Scale d r
type D3CategoryScale d r  = Scale d r
type D3QuantizeScale d r  = Scale d r
type D3QuantileScale d r  = Scale d r
type D3ThresholdScale d r = Scale d r

-- || The Continuous Scale Constructors
foreign import d3LinearScaleFn   :: ∀ d r eff. Effect (D3LinearScale d r)
foreign import d3LogScaleFn      :: ∀ d r eff. Effect (D3LogScale d r)
foreign import d3PowerScaleFn    :: ∀ d r eff. Effect (D3PowerScale d r)
foreign import d3IdentityScaleFn :: ∀ d r eff. Effect (D3IdentityScale d r)
foreign import d3TimeScaleFn     :: ∀ d r eff. Effect (D3TimeScale d r)

-- || The Ordinal Scale Constructors
foreign import d3BandScaleFn     :: ∀ d r eff. Effect                   (D3BandScale d r)
foreign import d3PointScaleFn    :: ∀ d r eff. Effect                   (D3PointScale d r)
foreign import d3CategoryScaleFn :: ∀ d r eff. EffectFn1 SchemeCategory (D3CategoryScale d r)

-- || Other Scale Constructors
foreign import d3QuantizeScaleFn :: ∀ d r eff. Effect                   (D3QuantizeScale d r)
foreign import d3QuantileScaleFn :: ∀ d r eff. Effect                   (D3QuantileScale d r)
foreign import d3ThresholdScaleFn :: ∀ d r eff. Effect                  (D3ThresholdScale d r)

-- functions
foreign import domainArrFn   :: ∀ d r eff. EffectFn2 (Array d)             (Scale d r) (Scale d r)
foreign import domainMapFn   :: ∀ d r eff. EffectFn2 (D3Map d)             (Scale d r) (Scale d r)
foreign import rangeFn       :: ∀ d r eff. EffectFn3 r r                   (Scale d r) (Scale d r)
foreign import rangeRoundFn  :: ∀ d r eff. EffectFn3 r r                   (Scale d r) (Scale d r)
foreign import roundFn       :: ∀ d r eff. EffectFn2 Boolean               (Scale d r) (Scale d r)
foreign import clampFn       :: ∀ d r eff. EffectFn2 Boolean               (Scale d r) (Scale d r)
foreign import interpolateFn :: ∀ d r eff. EffectFn2 Interpolator          (Scale d r) (Scale d r)
foreign import niceFn        :: ∀ d r eff. EffectFn1                       (Scale d r) (Scale d r)
foreign import nicePFn       :: ∀ d r eff. EffectFn2 Number                (Scale d r) (Scale d r)
foreign import invertFn      :: ∀ d r eff. EffectFn2 r                     (Scale d r) d
foreign import ticksFn       :: ∀ d r eff. EffectFn1                       (Scale d r) (Array d)
foreign import ticksPFn      :: ∀ d r eff. EffectFn2 Number                (Scale d r) (Array d)
foreign import tickFormatFn  :: ∀ d r eff. EffectFn2 Number                (Scale d r) (Number -> String)
foreign import tickFormatPFn :: ∀ d r eff. EffectFn3 Number Format         (Scale d r) (Number -> String)
foreign import applyScaleFn  :: ∀ d r v eff. EffectFn2 (Scale d r)         v            Number

-- || Foreign functions for the BandScale type
foreign import paddingFn         :: ∀ d r eff. EffectFn2 Number (D3BandScale d r) (D3BandScale d r)
foreign import paddingInnerFn    :: ∀ d r eff. EffectFn2 Number (D3BandScale d r) (D3BandScale d r)
foreign import paddingOuterFn    :: ∀ d r eff. EffectFn2 Number (D3BandScale d r) (D3BandScale d r)
foreign import bandwidthFn       :: ∀ d r eff. EffectFn1        (D3BandScale d r) Number

-- | Put at least a bit of structure on the construction of the Scales even tho they end up being untyped after creation
data ScaleType = Band
               | Category SchemeCategory
               | Identity
               | Linear
               | Log
               | Point
               | Power
               | Quantile
               | Quantize
               | Threshold
               | Time

d3Scale :: ∀ d r eff. ScaleType  -> Effect (Scale d r)
d3Scale (Category scheme) = runEffectFn1 d3CategoryScaleFn scheme
d3Scale Band       = d3BandScaleFn
d3Scale Identity   = d3IdentityScaleFn
d3Scale Linear     = d3LinearScaleFn
d3Scale Log        = d3LogScaleFn
d3Scale Point      = d3PointScaleFn
d3Scale Power      = d3PowerScaleFn
d3Scale Quantile   = d3QuantileScaleFn
d3Scale Quantize   = d3QuantizeScaleFn
d3Scale Threshold  = d3ThresholdScaleFn
d3Scale Time       = d3TimeScaleFn

-- || Scale functions, not all available to all Scales, caution! TODO
scaleBy :: ∀ d r v eff. Scale d r -> v -> Effect Number
scaleBy s = runEffectFn2 applyScaleFn s

-- sets the domain
domain :: ∀ d r eff. (D3Collection d) -> Scale d r         -> Effect(Scale d r)
domain (D3ArrT array)      = runEffectFn2 domainArrFn array
domain (D3MapT map)        = runEffectFn2 domainMapFn map
domain (D3Range start end) = runEffectFn2 domainArrFn [start, end]

-- sets the range (not necessarily numeric)
range :: ∀ d r eff. r -> r  -> Scale d r          -> Effect(Scale d r)
range = runEffectFn3 rangeFn

-- sets the range and sets rounding
rangeRound :: ∀ d r eff. r -> r  -> Scale d r     -> Effect(Scale d r)
rangeRound = runEffectFn3 rangeRoundFn

-- set rounding on or off
round :: ∀ d r eff. Boolean -> Scale d r           -> Effect(Scale d r)
round = runEffectFn2 roundFn

-- limit return to range (domain in case of invert)
clamp :: ∀ d r eff. Boolean -> Scale d r           ->Effect (Scale d r)
clamp = runEffectFn2 clampFn

-- specifies interpolator to use
interpolate :: ∀ d r eff. Interpolator -> Scale d r     ->Effect (Scale d r)
interpolate = runEffectFn2 interpolateFn

nice :: ∀ d r eff. (Maybe Number) -> Scale d r          -> Effect (Scale d r)
nice (Just count) = runEffectFn2 nicePFn count
nice Nothing      = runEffectFn1 niceFn

-- maps back from range to domain, NB invert can't be called on SequentialScales
invert :: ∀ d r eff. r -> (Scale d r)                ->  Effect d
invert = runEffectFn2 invertFn

-- get a list of n vals from domain
ticks :: ∀ d r eff. (Maybe Number) -> Scale d r       -> Effect (Array d)
ticks (Just n) = runEffectFn2 ticksPFn n
ticks Nothing  = runEffectFn1 ticksFn

tickFormat :: ∀ d r eff. Number -> Maybe Format -> Scale d r   -> Effect (Number -> String)
tickFormat n (Just f) = runEffectFn3 tickFormatPFn n f
tickFormat n Nothing  = runEffectFn2 tickFormatFn n


-- these functions should only apply to scales of type band, tricky to express the
-- way it's written right now...TODO revisit the types here
padding      :: ∀ d r eff. Number -> D3BandScale d r -> Effect (D3BandScale d r)
padding      = runEffectFn2 paddingFn

paddingInner :: ∀ d r eff. Number -> D3BandScale d r -> Effect (D3BandScale d r)
paddingInner = runEffectFn2 paddingInnerFn

paddingOuter :: ∀ d r eff. Number -> D3BandScale d r -> Effect (D3BandScale d r)
paddingOuter = runEffectFn2 paddingOuterFn

bandwidth    :: ∀ d r eff.           D3BandScale d r -> Effect Number
bandwidth    = runEffectFn1 bandwidthFn
