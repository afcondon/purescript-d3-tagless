module Main where

import Prelude (Unit, bind, pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import D3HOAS.Selection (class Selection, D3Structure(..), d3Select)

myD3Structure :: D3Structure
myD3Structure = D3S []

d3Script :: ∀ m. (Selection m) => m Unit
d3Script = do
    s  <- d3Select "foo"
    s2 <- d3Select "bar"
    pure s2
    -- pure $ merge s2

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do

  log "Hello sailor!"
