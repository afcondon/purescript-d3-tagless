module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import D3.Base (D3, D3Element)
import D3Impl (runD3Monad)
import Data.Int (toNumber)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Prelude (Unit, discard, bind, ($), (/))
import TaglessD3.AttrNew (Attr(..), attrFunction, attrValue, attributes)
import TaglessD3.Base (D3ElementType(SvgCircle, SvgGroup), D3Transition(NamedTransition), Duration(MS))
import TaglessD3.Selection (class AbstractSelection, D3Data(..), append, attrs, d3Select, selectAll, dataBind, enter, transition)

d3Script :: ∀ m. (AbstractSelection m) => m Unit
d3Script = do
    d3Select "#chart"
    append SvgGroup
    selectAll "text"
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
attrList     = attributes $ [ CX $ attrValue 20
                            , CY $ attrValue 20
                            , R  $ attrValue 20
                            , Style "width" $ attrValue "48%"
                            , Style "height" $ attrFunction lp ] -- shows callback but also demos like of typecheck on selection...

-- an example of a typed callback function
type ExData = { name :: String, age :: Int }

lp :: ExData -> Number -> Array D3Element -> D3Element -> Char
lp { name, age } _ _ _ =
    case name, age of -- silly little function just shows one way you might use an index function (NB in many cases D3 has better solutions for grouping)
    "awn", _ -> 'a'
    _, 0     -> 'b'
    _, _     -> 'c'


main :: forall e. Eff (console :: CONSOLE, d3 :: D3 | e) Unit
main = do
    _ <- runD3Monad d3Script Nothing
    log "\n\n\n====== cool beans =======\n\n\n"
    -- let ss = show $ S.runStructure d3Script mempty
    -- log ss
