module TaglessD3.Base where

import Color (Color)
import DOM.Event.Types (Event)
import Prelude (class Show, show, (<>))

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
        | SvgText String
        -- | SvgUse

instance showD3ElementType :: Show D3ElementType where
  show SvgCircle = "Circle"
  show SvgGroup  = "Group"
  show SvgImage  = "Image"
  show SvgPath   = "Path"
  show SvgRect   = "Rect"
  show (SvgText t) = "Text: " <> t

data Callback d b =   Lambda1 (d ->                                  b)
                    | Lambda2 (d -> Number ->                        b)
                    | Lambda4 (d -> Number -> Peers -> DomElement -> b)

data ValueOrCallback d b =  V b
                          | F (Callback d b)

instance showValueOrCallback :: (Show b) => Show (ValueOrCallback d b) where
  show (V b) = show b
  show (F _) = "Callback"

-- static evaluation of SVGPathString could be done in dummy interpreter or,
-- more ambitiously it could be built up using further DSL mechanics
type SVGPathString = String

-- ideally all of this attribute guff would be tagless as well
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

renderArrayOfAttributes :: ∀ d. Array (Attr d) -> String
renderArrayOfAttributes attrs = "[" <> show attrs <> "]"

instance showAttr :: Show (Attr d) where
    show (CX vcb)            = "CX: " <> show vcb
    show (CY vcb)            = "CY: " <> show vcb
    show (R vcb)             = "R: " <> show vcb
    show (X vcb)             = "X: " <> show vcb
    show (Y vcb)             = "Y: " <> show vcb
    show (DX vcb)            = "DX: " <> show vcb
    show (DY vcb)            = "DY: " <> show vcb
    show (Height vcb)        = "Height: " <> show vcb
    show (Width vcb)         = "Width: " <> show vcb
    show (StrokeWidth vcb)   = "StrokeWidth: " <> show vcb
    show (StrokeOpacity vcb) = "StrokeOpacity: " <> show vcb
    show (FillOpacity vcb)   = "FillOpacity: " <> show vcb
    show (Opacity vcb)       = "Opacity: " <> show vcb
    show (D vcb)             = "D: " <> show vcb
    show (Id vcb)            = "Id: " <> show vcb
    show (StrokeLineCap vcb) = "StrokeLineCap: " <> show vcb
    show (PatternUnits vcb)  = "PatternUnits: "  <> show vcb
    show (Style s vcb)       = "Style: " <> s <> ": " <> show vcb
    show (Class vcb)         = "Class: " <> show vcb
    show (Text vcb)          = "Text: " <> show vcb
    show (Type vcb)          = "Type: " <> show vcb
    show (Fill vcb)          = "Fill: " <> show vcb
    show (Stroke vcb)        = "Stroke: " <> show vcb
    show (Transform s)       = "Transform: " <> s
    show (EventHandlerS _ _) = "EventHandlerS: "
    show (EventHandlerN _ _) = "EventHandlerN: "
