module Main where

import TaglessD3.StringImpl (runStructure) as S
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import D3.Base (D3, D3Element)
import Data.Int (toNumber)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Prelude (Unit, bind, discard, show, ($), (*), (/))
import TaglessD3.AttrNew (Attr(..), AttrSetter, CssUnit(..), attrFunction, attrValue, attributes)
import TaglessD3.Base (D3ElementType(SvgCircle, SvgGroup), D3Transition(NamedTransition), Duration(MS))
import TaglessD3.D3Impl (runD3Monad)
import TaglessD3.Selection (class AbstractSelection, D3Data(ArrayD), append, attrs, d3Select, dataBind, enter, selectAll)

type D3Script = ∀ m. (AbstractSelection m) => m Unit

d3Script :: D3Script
d3Script = do
    d3Select "#chart"
    append SvgGroup
    selectAll "circle"
    dataBind myData'
    enter
    append SvgCircle
    attrs attrList
    -- transition myTransition

myTransition :: D3Transition
myTransition = NamedTransition "t1" $ MS 500

myData :: forall t8. D3Data Int t8
myData       = ArrayD [1,2,3,4,5] Nothing

myData' :: D3Data Int Number
myData'      = ArrayD [1,2,3,4,5,6,7,8] (Just \i -> (toNumber i) / 2.0) -- array data with lambda index fn

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
