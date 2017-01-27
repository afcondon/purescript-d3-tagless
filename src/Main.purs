module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import D3HOAS.Selection (class Selection, D3Structure(..), FakeSelection(..), d3Select, run, run')
import Data.Tuple (snd)

myD3Structure :: D3Structure
myD3Structure = D3S ["initial ", "D3", " Selection would be empty"]

d3Script :: ∀ m. (Selection m) => m Unit
d3Script = do
    s  <- d3Select "foo"
    s2 <- d3Select "bar"
    pure s2

-- foo = run' d3Script myD3Structure

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ run' d3Script myD3Structure
