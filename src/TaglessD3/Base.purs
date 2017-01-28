module TaglessD3.Base where

import Color (Color)
import DOM.Event.Types (Event)
import Data.Newtype (class Newtype)
import Prelude (class Show, show, (<>))
import Control.Apply (applySecond)

-- ||               Core foreign imports
-- the Effect of D3
foreign import data D3          :: !
-- the underlying D3 selection that is passed between calls
foreign import data D3Selection :: *
-- a Selection that's passed back in some callbacks
foreign import data Peers       :: *
-- the `this` pointer in a callback, DOM element receiving an event
foreign import data DomElement  :: *

-- can be used to provide closer match to the JavaScript D3 syntax if desired
infixl 4 applySecond as ..  -- maybe clearer than *>

type Selector = String

data Duration = Seconds Int | MS Int

data D3Transition = SimpleTransition Duration
                  | NamedTransition String Duration

newtype Hierarchy d = Hierarchy { name :: String, children :: Array (Hierarchy d), datum :: d }

derive instance newtypeHierarchy :: Newtype (Hierarchy d) _

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


-- static evaluation of SVGPathString could be done in dummy interpreter or,
-- more ambitiously it could be built up using further DSL mechanics
type SVGPathString = String

-- ideally all of this attribute guff would be tagless as well

-- there are a _lot_ of combinations hidden here by JavaScript's typeless nature (and coercions!)
-- attr Int
-- attr Number
-- attr String
-- attr Char
-- attr (d -> Int)
-- attr (d -> Number)
-- attr (d -> String)
-- attr (d -> Char)
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

instance showDuration :: Show Duration where
    show (Seconds i) = show i  <> "s"
    show (MS ms)     = show ms <> "ms"

instance showD3Transition :: Show D3Transition where
  show (SimpleTransition t)     = "Duration: " <> show t
  show (NamedTransition name t) = "Name: " <> name <> " Duration: " <> show t

instance showD3ElementType :: Show D3ElementType where
  show SvgCircle = "circle"
  show SvgGroup  = "group"
  show SvgImage  = "image"
  show SvgPath   = "path"
  show SvgRect   = "rect"
  show SvgText   = "text"

instance showValueOrCallback :: (Show b) => Show (ValueOrCallback d b) where
  show (V b) = " " <> show b
  show (F _) = " Callback"

-- 1) in keeping with D3 practice this is a mish-mash of HTML and SVG attributes,
-- which very probably should be rationalized better and perhaps taken from more
-- comprehensive libraries in Purescript which track the standards in question.
-- however, for now, it's useful simplification and gives us one single place to
-- put them all

-- 2) another idea entirely would be to investigate if the syntax here can be
-- made tagless too, in this way the 'vcb' could render differently in different
-- contexts too which is kind of what you'd expect / want

-- 3) another thought is that we really don't want a show instance here, what we
-- really want is to get back:
-- attrname | (attrname, value) | (attrname, callback)
instance showAttr :: Show (Attr d) where
    show (CX vcb)            = "cx" <> show vcb
    show (CY vcb)            = "cy" <> show vcb
    show (R vcb)             = "r" <> show vcb
    show (X vcb)             = "x" <> show vcb
    show (Y vcb)             = "y" <> show vcb
    show (DX vcb)            = "dx" <> show vcb
    show (DY vcb)            = "dy" <> show vcb
    show (Height vcb)        = "height" <> show vcb
    show (Width vcb)         = "width" <> show vcb
    show (StrokeWidth vcb)   = "strokewidth" <> show vcb
    show (StrokeOpacity vcb) = "strokeopacity" <> show vcb
    show (FillOpacity vcb)   = "fillopacity" <> show vcb
    show (Opacity vcb)       = "opacity" <> show vcb
    show (D vcb)             = "d" <> show vcb
    show (Id vcb)            = "id" <> show vcb
    show (StrokeLineCap vcb) = "strokelinecap" <> show vcb
    show (PatternUnits vcb)  = "patternunits"  <> show vcb
    show (Style s vcb)       = "style" <> s <> ": " <> show vcb
    show (Class vcb)         = "class" <> show vcb
    show (Text vcb)          = "text" <> show vcb
    show (Type vcb)          = "type" <> show vcb
    show (Fill vcb)          = "fill" <> show vcb
    show (Stroke vcb)        = "stroke" <> show vcb
    show (Transform s)       = "transform" <> s
    show (EventHandlerS _ _) = "EventHandlerS: "
    show (EventHandlerN _ _) = "EventHandlerN: "
