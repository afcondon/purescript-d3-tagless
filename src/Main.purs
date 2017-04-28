module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import D3.Base (D3)
import D3.Transition (D3Transition(TransitionName), DelaySetter(MilliSec), tStyle)
import Data.Int (toNumber)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Prelude (Unit, bind, discard, show, ($), (*), (/))
import TaglessD3.API (D3Data(ArrayDI, ArrayD), append, applyTransition, attrs, d3Select, dataBind, enter, makeTransition, selectAll, tAttrs, tDelay)
import TaglessD3.AttrNew (Attr(..), AttrSetter, CssUnit(..), attrFunction, attrValue, attributes)
import TaglessD3.Base (D3ElementType(SvgCircle, SvgGroup))
import TaglessD3.D3Impl (runD3Monad, D3Script)
import TaglessD3.StringImpl (runStructure) as S

d3Script :: D3Script
d3Script = do
    d3Select "#chart"
    append SvgGroup
    selectAll "circle" -- this select, the databind, the enter and the append could surely all be one function?
    dataBind myData'
    enter
    append SvgCircle
    attrs attrList
    applyTransition $ myTransition

-- idempotency of d3.transtion() this next relies on D3 to return us the same
-- transition as "name" (ie if named transition exists) allowing us to call this
-- function multiple times from different scripts that are under interpretation

-- now, if we want to add (common) attributes to this transition definition
-- we'll have to check if it already exists and not apply those attributes for
-- existing transitions
myTransition :: D3Script
myTransition = do
    makeTransition $ TransitionName "foo"
    tDelay $ MilliSec 2000.0
    tAttrs transitionAttrList

transitionAttrList :: List Attr
transitionAttrList = attributes $ [ Style "fill" $ attrValue "red" NoUnit ]

myData :: forall t8. D3Data Int t8
myData       = ArrayD [1,2,3,4,5]

myData' :: D3Data Int Number
myData'      = ArrayDI [1,2,3,4,5,6,7,8] (\i -> (toNumber i) / 2.0) -- array data with lambda index fn

attrList :: List Attr
attrList = attributes $ [ CX $ attrValue 20 Pt
                        , CY $ attrFunction (\d _ _ _ -> d * 3) Px
                        , R  $ attrFunction calcRadius Px
                        , Style "width" $ attrValue 48 Percent
                        , Style "height" $ attrFunction lp Px ] -- shows callback but also demos lack of typecheck on selection...

calcRadius :: AttrSetter Int Int
calcRadius d i n e = d * 2

-- an example of a typed callback function
type ExData = { name :: String, age :: Int }

-- lp :: ExData -> Number -> Array D3Element -> D3Element -> Int
lp :: AttrSetter ExData Int
lp { name, age } _ _ _ =
    case name, age of -- silly little function just shows one way you might use an index function (NB in many cases D3 has better solutions for grouping)
    "awn", _ -> 20
    _, 0     -> 50
    _, _     -> 100


main :: forall e. Eff (console :: CONSOLE, d3 :: D3 | e) Unit
main = do
    _ <- runD3Monad d3Script Nothing
    log "\n\n\n====== cool beans =======\n\n\n"
    let ss = show $ S.runStructure d3Script mempty
    log ss
