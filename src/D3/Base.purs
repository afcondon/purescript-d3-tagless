module D3.Base
  ( module Control.Monad.Eff
  , D3
  , D3Element
  , D3Selection
  , Peers
  , DomElement
  , D3Typenames
  , ListenerType(..)
  , Typenames(..)
  , Point
  , D3SetWithIndex
  , AttrSetter(..)
  , ClassSetter(..)
  , DataBind(..)
  , Filter(..)
  , PolyValue(..)
  , Index
  , Nodes
  , PredicateFn
  , PredicateB
  , PredicateS
  , PredicateN
  , transparent
  , opaque
  ) where

import Control.Monad.Eff (Eff)
import Data.Array ((:))
import Data.Foldable (foldr, intercalate)
import Data.Maybe (Maybe(Nothing, Just))
import Prelude (class Show, show, ($), (<>))

-- || FFI for D3
foreign import data D3 :: !
foreign import data D3Element :: *
-- the underlying D3 selection that is passed between calls
foreign import data D3Selection :: *
-- a Selection that's passed back in some callbacks
foreign import data Peers       :: *
-- the `this` pointer in a callback, DOM element receiving an event
foreign import data DomElement  :: *


type D3Eff a = ∀ e. Eff (d3 :: D3 | e) a

type Index       = Number
type D3Typenames = String
type Nodes       = Array D3Element
type Point       = { x :: Number, y :: Number }

-- | types for drags and zooms
data ListenerType = StartDrag | Drag | EndDrag
                  | StartZoom | Zoom | EndZoom

instance isShowListenerType :: Show ListenerType where
  show StartDrag = "start"
  show Drag      = "drag"
  show EndDrag   = "end"
  show StartZoom = "start"
  show Zoom      = "zoom"
  show EndZoom   = "end"

data Typenames = TypeNames (Array { name :: Maybe String, type :: ListenerType })

-- smush the Typenames down to a single string which D3 will (wastefully) parse out again)
instance isShowTypenames :: Show Typenames where
  show (TypeNames s) = intercalate " " $ foldr f [] s
    where
      f {name: (Just n), type: t } acc = ((show t) <> "." <> n) : acc
      f {name: Nothing,  type: t } acc =             (show t)  : acc

foreign import transparent :: String
foreign import opaque :: String

-- for selection.classed and selection.attr:
type PredicateFn    d x = ∀ eff. (d -> Number -> (Array D3Element) -> D3Element -> Eff (d3::D3|eff) x)
type PredicateB     d   = ∀ eff. (d -> Number -> (Array D3Element) -> D3Element -> Eff (d3::D3|eff) Boolean)
type PredicateS     d   = ∀ eff. (d -> Number -> (Array D3Element) -> D3Element -> Eff (d3::D3|eff) String)
type PredicateN     d   = ∀ eff. (d -> Number -> (Array D3Element) -> D3Element -> Eff (d3::D3|eff) Number)
type D3SetWithIndex d v = ∀ eff. (d -> Index -> Eff (d3::D3|eff) v)

-- | ADT used to wrap those polymorphic calls in D3 which take either
--      a value, or...
--      a function to get a value from the datum, or...
--      a function to get a value from the datum and its index
data DataBind d k = Data (Array d)
                  | Keyed (Array d) (d -> k)

data PolyValue d v  = Value v
                    | SetByIndex (D3SetWithIndex d v) -- really want to say d OR v is convertible to String - TODO EXPLORE

data Filter d       = Selector  String
                    | Predicate (d -> Boolean)

data ClassSetter  d = SetAll Boolean
                    | SetSome (PredicateB d)

data AttrSetter d x = SetAttr x
                    | AttrFn (PredicateFn d x)   -- rename both data ctor and Type here TODO

-- || Why so many of these ADTs - that's really gross and un-mnemonic, must fix TODO
