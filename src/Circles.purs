module Scripts.Circles (script) where

import TaglessD3.API (D3Data, append, applyTransition, attrs, d3Select, dataBind, delay, duration, enter, selectAll)
import D3.Transition (TimeSpec(..))
import Prelude (discard, ($), (*))
import TaglessD3.AttrNew (Attr(..), CssUnit(..), attrFunction, attrValue, attributes)
import TaglessD3.Base (D3ElementType(..))
import TaglessD3.D3Impl (D3Script)

script :: forall d i. D3Data d i -> D3Script -> D3Script
script ds t = do
    d3Select "#chart"
    append SvgGroup
    selectAll "circle" -- this select, the databind, the enter and the append could surely all be one function?
    dataBind ds
    enter
    append SvgCircle
    attrs $ attributes $ [ CX $ attrValue 200 Px
                         , CY $ attrFunction (\d _ _ _ -> d * 3) Px
                         , R  $ attrFunction (\d _ _ _ -> d * 5) Px
                         , Style "width" $ attrValue 48 Percent
                         , Style "height" $ attrFunction (\d _ _ _ -> d * 5) Px ]
    applyTransition t
    delay $ DelayFn (\d i -> i * 100.0)
    duration $ DelayFn (\d i -> i * 1000.0)
    attrs $ attributes $ [ Style "fill" $ attrValue "goldenrod" NoUnit
                         , CY $ attrFunction (\d _ _ _ -> d * 80) Px
                         ]
