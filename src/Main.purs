module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import D3.Base (D3)
import D3.Transition (D3Transition(TransitionName), TimeSpec(MilliSec))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Prelude (Unit, bind, discard, ($), (/))
import TaglessD3.API (D3Data(ArrayDI), delay, makeTransition)
import TaglessD3.D3Impl (runD3Monad, D3Script)
import TaglessD3.StringImpl (runStructure) as S

import Scripts.Rects as Rectangles
import Scripts.Circles as Circles

myTransition :: D3Script
myTransition = do
    makeTransition $ TransitionName "foo"
    delay $ MilliSec 2000.0

myData' :: D3Data Int Number
myData' = ArrayDI [1,2,3,4,5,6,7,8] (\i -> (toNumber i) / 2.0) -- array data with lambda index fn

main :: forall e. Eff (console :: CONSOLE, d3 :: D3 | e) Unit
main = do
    _ <- runD3Monad (Circles.script myData' myTransition) Nothing
    _ <- runD3Monad (Rectangles.script myData' myTransition) Nothing
    logShow $ S.runStructure (Circles.script myData' myTransition) mempty
