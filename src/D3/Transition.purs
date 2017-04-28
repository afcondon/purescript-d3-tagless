module D3.Transition
  ( D3DelayFn
  , TimeSpec(..)
  , AttrInterpolator(..)
  -- , (Selection d)
  , D3Transition(..)
  , d3Transition
  -- , addTransition
  , tAttr
  -- , call
  , delay
  , duration
  -- , each
  -- , ease
  , tEmpty
  , tFilter
  , tMerge
  , addTransition
  , makeTransition
  , namedTransition
  , savedTransition
  , tNode       -- implements D3 transition.node()
  , tNodes      -- implements D3 transition.nodes()
  -- , on
  , tRemove
  , tSelect
  , tSelectAll
  , makeSelection
  , tSize
  , tStyle      -- implements D3 transition.style() & D3 transition.styleTween
  -- , tText
  -- , tween
  ) where

-- || NB. These next functions are not implemented as separate functions - they're rolled up into tAttr and tStyle
  -- , attr
  -- , attrTween    -- rolled up (together with attr, style and styleTween) into tAttr & tStyle
  -- , style
  -- , styleTween

import Control.Monad.Eff.Uncurried (mkEffFn2, mkEffFn3, EffFn3, EffFn2, EffFn1, runEffFn3, runEffFn2, runEffFn1)
import D3.Base (D3Element, D3, Eff, Filter(..))
import D3.Interpolator (Time, D3TweenFn, D3TweenTarget, Index)
import D3.Selection (Selection)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe, Nullable)
import Prelude (class Show, pure, show, ($), (<$>), (<>))

foreign import d3TransitionFn    :: ∀ d eff.   EffFn1 (d3::D3|eff) String                                     (Selection d)
foreign import d3TransitionFn2   :: ∀ d eff.                                                 Eff (d3::D3|eff) (Selection d)

foreign import attrFn            :: ∀ d v eff. EffFn3 (d3::D3|eff) String v                    (Selection d) (Selection d)
foreign import durationFn        :: ∀ d eff.   EffFn2 (d3::D3|eff) Time                        (Selection d) (Selection d)
foreign import emptyFn           :: ∀ d eff.   EffFn1 (d3::D3|eff)                             (Selection d) Boolean
foreign import mergeFn           :: ∀ d eff.   EffFn2 (d3::D3|eff) (Selection d)               (Selection d) (Selection d)
foreign import namedTransitionFn :: ∀ d eff.   EffFn2 (d3::D3|eff) String                      (Selection d)  (Selection d)    -- changed from (s d) (t d)
foreign import nodeFn            :: ∀ d eff.   EffFn1 (d3::D3|eff)                             (Selection d) (Nullable D3Element)
foreign import nodesFn           :: ∀ d eff.   EffFn1 (d3::D3|eff)                             (Selection d) (Array D3Element)
foreign import styleFn           :: ∀ d v eff. EffFn3 (d3::D3|eff) String v                    (Selection d) (Selection d)
foreign import filterFn          :: ∀ d eff.   EffFn2 (d3::D3|eff) String                      (Selection d) (Selection d)
foreign import filterFnP         :: ∀ d eff.   EffFn2 (d3::D3|eff) (d -> Boolean)              (Selection d) (Selection d)
foreign import transitionFn      :: ∀ d eff.   EffFn1 (d3::D3|eff)                             (Selection d) (Selection d)    -- changed from (s d) (t d)
foreign import transition2Fn     :: ∀ d eff.   EffFn2 (d3::D3|eff) String                      (Selection d) (Selection d)
foreign import delayFn           :: ∀ d eff.   EffFn2 (d3::D3|eff) Time                        (Selection d) (Selection d)
foreign import removeFn          :: ∀ d eff.   EffFn1 (d3::D3|eff)                             (Selection d) (Selection d)
foreign import selectAllFn       :: ∀ d eff.   EffFn2 (d3::D3|eff) String                      (Selection d) (Selection d)
foreign import selectFn          :: ∀ d eff.   EffFn2 (d3::D3|eff) String                      (Selection d) (Selection d)
foreign import selectionFn       :: ∀ d eff.   EffFn1 (d3::D3|eff)                             (Selection d) (Selection d) -- changed from (t d) (s d)
foreign import sizeFn            :: ∀ d eff.   EffFn1 (d3::D3|eff)                             (Selection d) Int
foreign import textFn            :: ∀ d v eff. EffFn2 (d3::D3|eff) v                           (Selection d) (Selection d)

-- need to define types to clean these sigs up   TODO
foreign import durationIFn       :: ∀ d eff.   EffFn2 (d3::D3|eff)    (Fn2 d Number Time)                              (Selection d) (Selection d)
foreign import delayIFn          :: ∀ d eff.   EffFn2 (d3::D3|eff)    (Fn2 d Number Time)                              (Selection d) (Selection d)
foreign import attrIFn           :: ∀ d v eff. EffFn3 (d3::D3|eff)    String (EffFn3 (d3::D3|eff) d Index D3Element v) (Selection d) (Selection d)
foreign import styleIFn          :: ∀ d v eff. EffFn3 (d3::D3|eff)    String (EffFn3 (d3::D3|eff) d Index D3Element v) (Selection d) (Selection d)

foreign import styleTweenFn      :: ∀ d v eff. EffFn3 (d3::D3|eff)    String (EffFn3 (d3::D3|eff) d Index D3Element v) (Selection d) (Selection d)
foreign import textFnFn          :: ∀ d v v2 eff. EffFn2 (d3::D3|eff)        (EffFn2 (d3::D3|eff) v Index v2) (Selection d) (Selection d)

-- NB when using a saved transition you're going to be using it with different types of selections to this function will
-- morph it from type x to type d, the type of the selection to which it is being applied
foreign import savedTransitionFn :: ∀ d x eff. EffFn2 (d3::D3|eff) (Selection x)              (Selection d)  (Selection d)    -- changed from (s d) (t d)

type D3DelayFn d = d -> Index -> Time

data TimeSpec d = MilliSec Time
                | DelayFn (D3DelayFn d)

data AttrInterpolator d v =
      Target v  -- straightforward target final value to tween to using built-in interpolators
    | TweenTarget (D3TweenTarget v d) -- function which is called a single time to get a target final value
    | TweenFn     (D3TweenFn     v d) -- function which is called once to generate a function which is then
                                      -- called every tween frame to generate a value

data D3Transition = TransitionName String
                  | UnnamedTransition

instance showD3Transition :: Show D3Transition where
  show (TransitionName s) = "Named transition: " <> show s
  show UnnamedTransition  = "Unnamed transition"

d3Transition :: ∀ eff d. D3Transition -> Eff (d3::D3|eff) (Selection d) -- in fact, d is unknown for transitions, they're really Type -> Type
d3Transition (TransitionName name)    = runEffFn1 d3TransitionFn name
d3Transition UnnamedTransition        = runEffFn1 d3TransitionFn "noname" -- d3TransitionFn2

addTransition :: ∀ d eff.  String -> Selection d  -> Eff (d3::D3|eff) (Selection d)
addTransition               = runEffFn2 transition2Fn

makeSelection :: ∀ d eff.                             Selection d  -> Eff (d3::D3|eff) (Selection d)   -- changed from (t d) Eff (s d)
makeSelection              = runEffFn1 selectionFn

makeTransition :: ∀ d eff.                             Selection d  -> Eff (d3::D3|eff) (Selection d)  -- changed from (s d) Eff (t d)
makeTransition              = runEffFn1 transitionFn

namedTransition :: ∀ d eff. String                  -> Selection d  -> Eff (d3::D3|eff) (Selection d)  -- changed from (s d) Eff (t d)
namedTransition name        = runEffFn2 namedTransitionFn name

savedTransition :: ∀ d x eff. (Selection x)        -> Selection d  -> Eff (d3::D3|eff) (Selection d)  -- changed from (s d) Eff (t d)
savedTransition name        = runEffFn2 savedTransitionFn name

duration :: ∀ d eff. TimeSpec d                    -> Selection d -> Eff (d3::D3|eff) (Selection d)
duration (MilliSec t)       = runEffFn2 durationFn t
duration (DelayFn f)        = runEffFn2 durationIFn (mkFn2 f)

delay :: ∀ d eff. TimeSpec d                      -> Selection d -> Eff (d3::D3|eff) (Selection d)
delay (MilliSec t)          = runEffFn2 delayFn t
delay (DelayFn f)           = runEffFn2 delayIFn (mkFn2 f)

tEmpty :: ∀ d eff.                                     Selection d -> Eff (d3::D3|eff) Boolean
tEmpty                      = runEffFn1 emptyFn

tFilter  :: ∀ d eff.  Filter d                      -> Selection d -> Eff (d3::D3|eff) (Selection d)
tFilter (Selector s)       = runEffFn2 filterFn s
tFilter (Predicate p)      = runEffFn2 filterFnP p

tNode :: ∀ d eff.                                      Selection d -> Eff (d3::D3|eff) (Maybe D3Element)
tNode t                     = toMaybe <$> runEffFn1 nodeFn t

tNodes :: ∀ d eff.                                     Selection d -> Eff (d3::D3|eff) (Array D3Element)
tNodes                      = runEffFn1 nodesFn

tMerge :: ∀ d eff.    Selection d                  -> Selection d -> Eff (d3::D3|eff) (Selection d)
tMerge                      = runEffFn2 mergeFn

tRemove :: ∀ d eff.                                    Selection d -> Eff (d3::D3|eff) (Selection d) -- maybe this should be Void? TODO
tRemove                     = runEffFn1 removeFn

tSize :: ∀ d eff.                                      Selection d -> Eff (d3::D3|eff) Int
tSize                       = runEffFn1 sizeFn

tSelectAll :: ∀ d eff. String                        -> Selection d -> Eff (d3::D3|eff) (Selection d)
tSelectAll selector           = runEffFn2 selectAllFn selector

tSelect  :: ∀ d eff.  String                         -> Selection d -> Eff (d3::D3|eff) (Selection d)
tSelect selector              = runEffFn2 selectFn selector

-- replace this with same scheme as for Attributes TODO
-- tText  :: ∀ d v eff.  PolyValue d v                  -> Selection d -> Eff (d3::D3|eff) (Selection d)
-- tText       (Value value)     = runEffFn2 textFn value
-- tText       (SetByIndex f)    = runEffFn2 textFnFn (mkEffFn2 f)


tAttr :: ∀ d v eff. String -> AttrInterpolator d v  -> Selection d -> Eff (d3::D3|eff) (Selection d)
tAttr name (Target v)       = runEffFn3 attrFn       name v
tAttr name (TweenTarget f)  = runEffFn3 attrIFn      name (mkEffFn3 f)
tAttr name (TweenFn f)      = runEffFn3 styleTweenFn name (mkEffFn3 f)

tStyle :: ∀ d v eff. String -> AttrInterpolator d v -> Selection d -> Eff (d3::D3|eff) (Selection d)
tStyle name (Target v)      = runEffFn3 styleFn      name v
tStyle name (TweenTarget f) = runEffFn3 styleIFn     name (mkEffFn3 f)
tStyle name (TweenFn f)     = runEffFn3 styleTweenFn name (mkEffFn3 f)
