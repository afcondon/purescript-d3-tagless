module TaglessD3.Base where

import Color (Color)
import DOM.Event.Types (Event)

-- ||               Core foreign imports
-- the Effect of D3
foreign import data D3          :: !
-- the underlying D3 selection that is passed between calls
foreign import data D3Selection :: *
-- a Selection that's passed back in some callbacks
foreign import data Peers       :: *
-- the `this` pointer in a callback, DOM element receiving an event
foreign import data DomElement  :: *

type Selector = String

data Duration = Seconds Int | MS Int
data D3Transition = SimpleTransition Duration
                  | NamedTransition String Duration

data D3ElementType
    =     SvgCircle
        -- | SvgEllipse
        | SvgGroup
        | SvgImage
        -- | SvgLine
        -- | SvgMesh
        | SvgPath
        -- | SvgPolygon
        -- | SvgPolyline
        | SvgRect
        | SvgText
        -- | SvgUse

data Callback d b =   Lambda1 (d ->                                  b)
                    | Lambda2 (d -> Number ->                        b)
                    | Lambda4 (d -> Number -> Peers -> DomElement -> b)

data ValueOrCallback d b =  V b
                          | F (Callback d b)

type SVGPathString = String -- could validate this here at least with runtime constructor

data Attr d  = CX                  (ValueOrCallback d Number)  -- circles only
             | CY                  (ValueOrCallback d Number)  -- circles only
             | R                   (ValueOrCallback d Number)  -- circles only
             | X                   (ValueOrCallback d Number)
             | Y                   (ValueOrCallback d Number)
             | DX                  (ValueOrCallback d String) -- string because it might have units with
             | DY                  (ValueOrCallback d String)
             | Height              (ValueOrCallback d Number)
             | Width               (ValueOrCallback d Number)
             | StrokeWidth         (ValueOrCallback d Number)
             | StrokeOpacity       (ValueOrCallback d Number)
             | FillOpacity         (ValueOrCallback d Number)
             | Opacity             (ValueOrCallback d Number)
             | D                   (ValueOrCallback d SVGPathString)
             | Id                  (ValueOrCallback d String)
             | StrokeLineCap       (ValueOrCallback d String) -- Round | Butt | Square
             | PatternUnits        (ValueOrCallback d String) -- userSpaceOnUse | objectBoundingBox
             | Style        String (ValueOrCallback d String)
             | Class               (ValueOrCallback d String)
             | Text                (ValueOrCallback d String)
             | Type                (ValueOrCallback d String) -- images only
             | Fill                (ValueOrCallback d Color)
             | Stroke              (ValueOrCallback d Color)
             | Transform    String  -- would be nice to build this up from ADT too
             | EventHandlerS Event (Callback d String) -- click | mouseenter | mouseleave | mouseover
             | EventHandlerN Event (Callback d Number)
