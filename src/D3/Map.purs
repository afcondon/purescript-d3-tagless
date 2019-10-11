module D3.Collections.Map
  ( d3MapFn
  , D3Map
  , D3MapKey
  -- , D3MapValue
  , D3KeyValue
  , D3KVFn
  , d3Map
  , d3MapF
  , d3mapGetFn
  , d3mapSetFn
  , d3mapHasFn
  , d3mapRemoveFn
  , d3mapClearFn
  , d3mapKeysFn
  , d3mapValuesFn
  , d3mapEntriesFn
  , d3mapSizeFn
  , d3mapEmptyFn
  , d3mapEachFn
  ) where

import Prelude
import Effect
import Effect.Uncurried ( EffectFn2, EffectFn1, runEffectFn1, runEffectFn2)

foreign import data D3Map :: Type -> Type

type D3MapKey     = String
-- type D3MapValue   = Void -- d3Maps can have varying types in one map
type D3KeyValue d  = { key :: D3MapKey, value :: d }
type D3KVFn       = ∀ d eff. EffectFn2 D3MapKey d Unit
-- type D3MakeKeysEffectFn d = ∀ eff. EffectFn1 d          D3MapKey
-- type D3MakeKeysFn d    = ∀ eff. d          -> Effect D3MapKey
--
-- test :: ∀ d eff. D3MakeKeysFn d -> D3MakeKeysEffectFn d
-- test = mkEffectFn1
--
-- test' :: ∀ d eff. D3MakeKeysEffectFn d -> D3MakeKeysFn d
-- test' = runEffectFn1

-- | Constructor for a D3Map that takes a collection and tries to make Map from it
-- | since this can fail it would be good to add EXCEPTION here and return EITHER - TODO
foreign import d3MapFn        :: ∀ c d eff. EffectFn1 c                (D3Map d)
foreign import d3MapFnFn      :: ∀ c d eff. EffectFn2
                                                    c
                                                    (d -> D3MapKey) -- function passed in
                                                    (D3Map d)


-- | Constructor for a D3Map that takes a collection and a function to produce keys from the data
-- foreign import d3MapFnFn      :: ∀ c d eff. EffectFn2 c  (D3MakeKeysEffectFn d)  (D3Map d)

-- | Functions available on D3Maps, similar but not same to ES5 Maps
foreign import d3mapClearFn   :: ∀ d eff. EffectFn1 (D3Map d)          (D3Map d)
foreign import d3mapEmptyFn   :: ∀ d eff. EffectFn1 (D3Map d)          Boolean
foreign import d3mapEntriesFn :: ∀ d eff. EffectFn1 (D3Map d)          (Array (D3KeyValue d))
foreign import d3mapGetFn     :: ∀ d eff. EffectFn1 D3MapKey           d
foreign import d3mapKeysFn    :: ∀ d eff. EffectFn1 (D3Map d)          (Array D3MapKey)
foreign import d3mapSizeFn    :: ∀ d eff. EffectFn1 (D3Map d)          Number
foreign import d3mapValuesFn  :: ∀ d eff. EffectFn1 (D3Map d)          (Array d)
foreign import d3mapHasFn     :: ∀ d eff. EffectFn2 D3MapKey (D3Map d) Boolean
foreign import d3mapRemoveFn  :: ∀ d eff. EffectFn2 D3MapKey (D3Map d) (D3Map d)
foreign import d3mapEachFn    :: ∀ d eff. EffectFn2 D3KVFn   (D3Map d) (D3Map d)
foreign import d3mapSetFn     :: ∀ d eff. EffectFn2 D3MapKey    d      (D3Map d)

-- | Purescript constructor for D3Map
-- Lot of type unsafety here - we're giving the D3 map constructor a type c and claiming it
-- gives us back a Map of type d, assumes c is a valid homomorphous collection of d's
d3Map :: ∀ c d eff. c -> Effect (D3Map d)
d3Map c  = runEffectFn1 d3MapFn c
-- | Also, this callback does potentially add real overhead since it's called
-- for all elements of the map but it's gotta be done this way if you're going
-- to pass in a Purescript lambda (maybe you could add a third variation that
-- takes an FFI function instead of passing Lambda)
d3MapF :: ∀ c d eff. c -> (d -> D3MapKey) -> Effect (D3Map d)
d3MapF c f = runEffectFn2 d3MapFnFn c f

-- | Functions available on D3Maps, similar but not same to ES5 Maps
d3mapClear   :: ∀ d eff. (D3Map d)                  -> Effect (D3Map d)
d3mapClear = runEffectFn1 d3mapClearFn

d3mapEach    :: ∀ d eff. D3KVFn -> (D3Map d)        -> Effect (D3Map d)
d3mapEach = runEffectFn2 d3mapEachFn

d3mapEmpty   :: ∀ d eff. (D3Map d)                  -> Effect Boolean
d3mapEmpty = runEffectFn1 d3mapEmptyFn

d3mapEntries :: ∀ d eff. (D3Map d)                  -> Effect (Array (D3KeyValue d))
d3mapEntries = runEffectFn1 d3mapEntriesFn

d3mapGet     :: ∀ d eff. D3MapKey                   -> Effect  d
d3mapGet = runEffectFn1 d3mapGetFn

d3mapHas     :: ∀ d eff. D3MapKey -> (D3Map d)      -> Effect Boolean
d3mapHas = runEffectFn2 d3mapHasFn

d3mapKeys    :: ∀ d eff. (D3Map d)                  -> Effect (Array D3MapKey)
d3mapKeys = runEffectFn1 d3mapKeysFn

d3mapRemove  :: ∀ d eff. D3MapKey -> (D3Map d)      -> Effect (D3Map d)
d3mapRemove = runEffectFn2 d3mapRemoveFn

d3mapSet     :: ∀ d eff. D3MapKey -> d              -> Effect (D3Map d)
d3mapSet = runEffectFn2 d3mapSetFn

d3mapSize    :: ∀ d eff. (D3Map d)                  -> Effect Number
d3mapSize = runEffectFn1 d3mapSizeFn

d3mapValues  :: ∀ d eff. (D3Map d)                  -> Effect (Array d)
d3mapValues = runEffectFn1 d3mapValuesFn
