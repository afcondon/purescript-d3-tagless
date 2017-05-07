module Scripts.Rects (script) where

import TaglessD3.API (D3Data, append, applyTransition, attrs, d3SelectAll, dataBind, delay, duration, enter, selectAll)
import D3.Transition (TimeSpec(..))
import Data.List (List)
import Prelude (discard, ($), (*))
import TaglessD3.AttrNew (Attr(..), CssUnit(..), attrFunction, attrValue, attributes)
import TaglessD3.Base (D3ElementType(..))
import TaglessD3.D3Impl (D3Script)

script :: ∀ d i. D3Data d i -> D3Script -> D3Script
script ds t = do
    d3SelectAll "#chart"
    append SvgGroup
    selectAll "rect" -- this select, the databind, the enter and the append could surely all be one function?
    dataBind ds
    enter
    append SvgRect
    attrs myAttrs
    applyTransition t
    delay $ DelayFn (\d i -> i * 100.0)
    duration $ DelayFn (\d i -> i * 1000.0)
    attrs transitionAttrs

myAttrs :: List Attr
myAttrs = attributes $ [ Y $ attrValue 100 Px
                         , X $ attrFunction (\d _ _ _ -> d * 30) Px
                         , Width  $ attrFunction (\d _ _ _ -> d * 10) Px
                         , Height  $ attrFunction (\d _ _ _ -> d * 10) Px]

transitionAttrs :: List Attr
transitionAttrs = attributes $ [ Style "fill" $ attrValue "hotpink" NoUnit
                                  , X $ attrFunction (\d _ _ _ -> d * 80) Px
                                  ]
