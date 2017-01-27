module Main where

import Prelude (Unit, bind, pure, ($))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import TaglessD3.Selection --(class Selection, D3Structure(..), FakeSelection(..), d3Select, run, run')

myD3Structure :: D3Structure
myD3Structure = initD3S "Awn"

myD3Structure2 :: D3Structure
myD3Structure2 = initD3S "Bel"

d3Script2 :: ∀ m. (Selection m) => m Unit
d3Script2 = d3Select "quux"


d3Script :: ∀ m. (Selection m) => m Unit
d3Script = do
    s  <- d3Select "foo"
    s2 <- select "bar"
    s3 <- selectAll "baz"
    s4 <- merge d3Script2 myD3Structure2
    pure s3

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ run' d3Script myD3Structure
