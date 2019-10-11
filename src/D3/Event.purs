module D3.Event where


import Effect
import Web.Event.Event (Event)
import Web.TouchEvent.TouchEvent (TouchEvent)
import Web.HTML.HTMLElement (HTMLElement)
import Effect.Uncurried (runEffectFn3, runEffectFn1, EffectFn3, EffectFn1, EffectFn2, runEffectFn2)
import Data.Nullable (Nullable)
import Prelude (Unit)

foreign import d3EventFn :: Unit -> Effect Event

type D3Point = Array Number -- in reality only two elements [x,y]

type HTMLContainer = HTMLElement -- see no way of constraining this at the moment, i think

-- gets the {x,y} relative to a specified container (such as HTML or SVG element)
foreign import d3MouseFn :: HTMLContainer -> Effect D3Point

-- || NB all this touch stuff is very sketchy and unimplemented

-- Returns the x and y coordinates of the touch with the specified identifier
-- associated with the current event relative to the specified container.
type D3Touch      = TouchEvent -- not sure that this is right, but it can be a placeholder
type TouchID      = String
type TouchHistory = Array (Array Number)   -- of the form  [[x1,y1], [x2,y2]]

foreign import d3TouchFn          :: EffectFn3 HTMLContainer (Array D3Touch) TouchID    (Nullable TouchHistory)
foreign import d3TouchDefaultFn   :: EffectFn2 HTMLContainer                 TouchID    (Nullable TouchHistory)
foreign import d3TouchesFn        :: EffectFn2 HTMLContainer (Array D3Touch)            TouchHistory
foreign import d3TouchesDefaultFn :: EffectFn1 HTMLContainer                            TouchHistory

-- # d3.touch(container[, touches], identifier)
d3Touch :: HTMLContainer -> Array D3Touch -> TouchID -> Effect (Nullable TouchHistory)
d3Touch = runEffectFn3 d3TouchFn

d3TouchDefault :: HTMLContainer -> TouchID           -> Effect (Nullable TouchHistory)
d3TouchDefault = runEffectFn2 d3TouchDefaultFn

-- # d3.touches(container[, touches])
d3Touches        :: HTMLContainer -> Array D3Touch -> Effect TouchHistory
d3Touches        = runEffectFn2 d3TouchesFn

d3TouchesDefault :: HTMLContainer                  -> Effect TouchHistory
d3TouchesDefault = runEffectFn1 d3TouchesDefaultFn
