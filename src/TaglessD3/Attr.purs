module TaglessD3.Attr where

import Prelude
import TaglessD3.Base (SVGPathString)
import D3.Base (DomElement, Peers)
import Color (Color)
import DOM.Event.Types (Event)

data ValueOrCallback d b =  V b
                          | F (Callback d b)

instance showValueOrCallback :: (Show b) => Show (ValueOrCallback d b) where
  show (V b) = " " <> show b
  show (F _) = " Callback"



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
