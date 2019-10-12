module D3.Drag where

import Effect
import D3.Base (D3Element, Typenames, Index, D3Typenames)
import Web.Event.Event (Event)
import Data.Array ((:))
import Effect.Uncurried (EffectFn3, EffectFn2, runEffectFn2, runEffectFn3)
import Data.Nullable (Nullable)
import Prelude (Unit, show)

foreign import data Drag :: Type -> Type

foreign import d3DragFn :: ∀ d. Effect (Drag d)

-- When a drag event listener is invoked, d3.event is set to the current drag event.
foreign import d3DragEvent :: ∀ d. Unit -> Effect (DragEvent d)

type DragEvent d = {
    target      :: Drag d
  , type        :: String  -- either "start", "drag" or "end"
  , subject     :: Subject -- the drag subject, defined by drag.subject.
  , x           :: Number  -- the new x-coordinate of the subject
  , y           :: Number  -- the new y-coordinate of the subject
  , dx          :: Number  -- the change in x-coordinate since the previous drag event.
  , dy          :: Number  -- the change in y-coordinate since the previous drag event.
  , identifier  :: String  -- either "mouse" or numeric touch identifier
  , active      :: Number  -- the number of currently active drag gestures (on start and end, not including this one).
  , sourceEvent :: Event
}

type Draggable r = { x :: Number, y :: Number | r }  -- minimum requirement for draggable object

type DragListener d = (d -> Number -> Array D3Element -> D3Element -> Effect Unit)

-- create a new Drag behaviour Object/Function thingy
d3Drag :: ∀ d.  d -> Effect (Drag d)
d3Drag d = d3DragFn

-- || Going to try for a slightly richer, more PureScript-y API here just
-- because the whole polymorphic dispatch / apply on business is so confusing
-- and fluid and we're going end up making it explicit in the argument if not
-- the function so might as well bite the bullet and define some additional API
-- points
-- || i'd like to come back to this once drag and zoom are working and see whether
-- || it makes sense to wrap dispatch itself TODO

foreign import findCallbackFn    :: ∀ d. EffectFn2 D3Typenames (Drag d) (Nullable (DragListener d))
foreign import removeListenersFn :: ∀ d. EffectFn2 D3Typenames (Drag d) (Drag d)
-- foreign import applyDragFn       :: ∀ d. EffectFn2 (Drag d) (Selection d) (Selection d)
foreign import dragUpdateFn      :: ∀ d. EffectFn2 d D3Element Unit


foreign import addListenerFn     :: ∀ d. EffectFn3 D3Typenames
                                        (EffectFn3PlusThis d Number (Array D3Element) Unit)
                                        (Drag d)
                                        (Drag d)

foreign import data EffectFn3PlusThis :: Type -> Type -> Type -> Type -> Type    -- JS call with three params
foreign import mkEffectFn4Special :: forall d r.
                                        (d -> Index -> Array D3Element -> D3Element -> Effect r) -- callback has 4 params
                                        -> EffectFn3PlusThis d Index (Array D3Element) Unit      -- JS calls with 3 params + this

-- lookup and remove differ in JS as listeners-not-given => lookup, listeners-as-null => remove
lookupDrag      :: ∀ d. Typenames -> Drag d -> Effect (Nullable (DragListener d))
lookupDrag tn = runEffectFn2 findCallbackFn (show tn)

removeDragListeners :: ∀ d. Typenames -> Drag d -> Effect (Drag d)
removeDragListeners tn = runEffectFn2 removeListenersFn (show tn)

addDragListener     :: ∀ d. Typenames -> DragListener d   -> Drag d -> Effect (Drag d)
-- callback has 4 params but D3 will be calling it with 3 params and hiding the 4th in the `this` pointer
addDragListener tn callback = runEffectFn3 addListenerFn (show tn) (mkEffectFn4Special callback)

-- applyDrag - not used in current commit, this was an attempt to avoid the unsafeCoerce on the drag - TODO
-- applyDrag :: ∀ d. (Drag d) -> (Selection d) -> Effect (Selection d)
-- applyDrag = runEffectFn2 applyDragFn

-- slightly suspect side-effecting function to update the dragged object Both
-- the DOM element and the data element need to change but the D3 way of doing
-- this is to set the data element's fields while changing the `attr` of the DOM
-- element and that's what this function wraps
dragUpdate :: ∀ d. d -> D3Element -> Effect Unit
dragUpdate = runEffectFn2 dragUpdateFn

-- on :: ∀ d. (d -> Effect(unit)) -> Drag d -> Effect Drag
-- on dragFn

{-
  drag.on = function() {
      var value = listeners.on.apply(listeners, arguments); [1]
      return value === listeners ? drag : value;
    };

// [1]
  function(typename, callback) {
      var _ = this._,
          T = parseTypenames(typename + "", _),
          t,
          i = -1,
          n = T.length;

      // If no callback was specified, return the callback of the given type and name.
      if (arguments.length < 2) {
        while (++i < n) if ((t = (typename = T[i]).type) && (t = get(_[t], typename.name))) return t;
        return;
      }

      // If a type was specified, set the callback for the given type and name.
      // Otherwise, if a null callback was specified, remove callbacks of the given name.
      if (callback != null && typeof callback !== "function") throw new Error("invalid callback: " + callback);
      while (++i < n) {
        if (t = (typename = T[i]).type) _[t] = set$2(_[t], typename.name, callback);
        else if (callback == null) for (t in _) _[t] = set$2(_[t], typename.name, null);
      }

      return this;
    }

  -}

type AccessorFn d = d -> Effect Unit

foreign import data Subject :: Type

-- data SubjectOpts d = GetAccessor
--                    | SetAccessorFn (d -> ?)
--                    | SetAccessorObj Draggable
{-
  The subject of a drag gesture represents the thing being dragged. It is computed
  when an initiating input event is received, such as a mousedown or touchstart,
  immediately before the drag gesture starts. The subject is then exposed as
  event.subject on subsequent drag events for this gesture.
-}

-- if subject specified sets the subject accessor to the specified object or function and returns the drag behavior.
-- subject :: ∀ d. SubjectOpts d ->

-- If subject is not specified, returns the current subject accessor, which defaults to:
{-
    function subject(d) {
      return d == null ? {x: event.x, y: event.y} : d;
    }
-}


-- sets the filter to the specified function and returns the drag behavior.
-- filter ::

-- sets the container accessor to the specified object or function and returns the drag behavior
-- container ::
