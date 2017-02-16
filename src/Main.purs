module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import D3.Base (D3)
import D3Impl (runD3Monad)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Number (divide)
import Prelude (Unit, bind, id, show, ($), (*>), (>>=), show)
import TaglessD3.AttrNew (Attr(..), attrCharP, attrInt, attrString, attributes)
import TaglessD3.Base (D3ElementType(..), D3Transition(..), Duration(..), (..))
import TaglessD3.Selection (class AbstractSelection, D3Data(..), append, attrs, d3Select, dataBind, enter, transition)
import TaglessD3.StringImpl (runStructure) as S


d3Script :: ∀ m. (AbstractSelection m) => m Unit
d3Script = do
            d3Select "#chart"
            append HTMLDiv

d3Script' :: ∀ m. (AbstractSelection m) => m Unit
d3Script' = do
            d3Select "#chart"
            dataBind myData'
            enter
            append SvgText
            attrs attrList
            transition myTransition

myTransition = NamedTransition "t1" $ MS 500
myData       = ArrayD [1,2,3,4,5] Nothing
myData'      = ArrayD [1,2,3,4,5] (Just \i -> divide (toNumber i) 2.0) -- array data with lambda index fn
attrList     = attributes $ [ CX $ attrInt 1
                            , Style "width" $ attrString "48%"
                            , Style "height" $ attrCharP lp ] -- shows callback but also demos like of typecheck on selection...

-- an example of a typed callback function
type ExData = { name :: String, age :: Int }

lp :: ExData -> Char
lp { name, age } =
    case name, age of
    "awn", _ -> 'a'
    _, 0     -> 'b'
    _, _     -> 'c'


main :: forall e. Eff (console :: CONSOLE, d3 :: D3 | e) Unit
main = do
    runD3Monad d3Script Nothing
    log "\n\n\n====== cool beans =======\n\n\n"
    let ss = show $ S.runStructure d3Script' mempty
    log ss
