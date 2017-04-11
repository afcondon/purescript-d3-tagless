module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import D3.Base (D3)
import D3Impl (runD3Monad)
import Data.Int (toNumber)
import Data.List (List)
import Data.Maybe (Maybe(..))
-- import Number (divide)
import Prelude (Unit, discard, bind, ($), (/))
import TaglessD3.AttrNew (Attr(..), attrCharP, attrInt, attrString, attributes)
import TaglessD3.Base (D3ElementType(SvgText, SvgGroup), D3Transition(NamedTransition), Duration(MS))
import TaglessD3.Selection (class AbstractSelection, D3Data(..), append, attrs, d3Select, dataBind, enter, transition)

d3Script :: ∀ m. (AbstractSelection m) => m Unit
d3Script = do
    d3Select "#chart"
    append SvgGroup
    dataBind myData'
    enter
    append SvgText
    attrs attrList
    transition myTransition

myTransition :: D3Transition
myTransition = NamedTransition "t1" $ MS 500

myData :: forall t8. D3Data Int t8
myData       = ArrayD [1,2,3,4,5] Nothing

myData' :: D3Data Int Number
myData'      = ArrayD [1,2,3,4,5] (Just \i -> (toNumber i) / 2.0) -- array data with lambda index fn

attrList :: List Attr
attrList     = attributes $ [ CX $ attrInt 1
                            , Style "width" $ attrString "48%"
                            , Style "height" $ attrCharP lp ] -- shows callback but also demos like of typecheck on selection...

-- an example of a typed callback function
type ExData = { name :: String, age :: Int }

lp :: ExData -> Char
lp { name, age } =
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
