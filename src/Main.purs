module Main where

import TaglessD3.Selection
import Control.Apply (applySecond)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Prelude (Unit, ($))
import TaglessD3.Base (Attr(..), D3ElementType(..), ValueOrCallback(..))

infixl 4 applySecond as ..

myD3Structure :: D3Structure
myD3Structure = initD3S "Awn"

myD3Structure2 :: D3Structure
myD3Structure2 = initD3S "Bel"



d3Script2 :: ∀ m. (Selection m) => m Unit
d3Script2 = d3Select "quux"

d3Script' :: ∀ m. (Selection m) => m Unit
d3Script' = d3Select "div"
         .. enter
         .. append (SvgText "Loren ipsum...")
         .. attrs [ CX (V 2.0) ]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ run' d3Script' myD3Structure
