module D3.Zoom (
      Zoom
    , ZoomEvent
    , Transform
    , Extent
    , d3Zoom
    , getZoomTransform
    , getZoomEvent
    , scaleExtent
    , addZoomListener
  ) where

import D3.Base (D3Element, D3Typenames, D3, Eff, Typenames)
import D3.Drag (EffectFn3PlusThis, DragListener, mkEffectFn4Special)
import Web.Event.Event (Event)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Prelude (Unit, show)

foreign import data Zoom :: Type

type Extent = Array Number -- of `length` 2, just the min and max zoom

-- When a zoom event listener is invoked, d3.event is set to the current drag event.
foreign import d3ZoomEventFn  :: ∀ eff. Effect ZoomEvent
foreign import getTransformFn :: ∀ eff. Effect Transform

type Transform = { k :: Number, x :: Number, y:: Number }
type ZoomEvent = {
    target      :: Zoom
  , type        :: String
  , transform   :: Transform
  , sourceEvent :: Event
}

foreign import d3ZoomFn       :: ∀ eff. Eff    (d3::D3|eff)             Zoom
foreign import scaleExtentFn  :: ∀ eff. EffectFn2 Extent Zoom Zoom
foreign import addZoomListenerFn  :: ∀ d eff. EffectFn3 D3Typenames
                                                              (EffectFn3PlusThis (d3::D3|eff) d Number (Array D3Element) Unit)
                                                              Zoom
                                                              Zoom

d3Zoom :: ∀ eff. Effect Zoom
d3Zoom = d3ZoomFn

scaleExtent :: ∀ eff. Extent -> Zoom -> Effect Zoom
scaleExtent = runEffectFn2 scaleExtentFn

addZoomListener :: ∀ d eff. Typenames -> DragListener d -> Zoom -> Effect Zoom
addZoomListener tn callback = runEffectFn3 addZoomListenerFn (show tn) (mkEffectFn4Special callback)

getZoomTransform :: ∀ eff. Effect Transform
getZoomTransform = getTransformFn

getZoomEvent :: ∀ eff. Effect ZoomEvent
getZoomEvent = d3ZoomEventFn
