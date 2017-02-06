module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import D3.Base (D3, D3Selection)
import TaglessD3.Attr -- (Attr(..))
import TaglessD3.AttrNew (main2)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Number (divide)
import Prelude (Unit, bind, id, pure, unit, ($))
import TaglessD3.Base (D3ElementType(..), D3Transition(..), Duration(..), (..))
import TaglessD3.DOMImpl (initD3Selection)
import TaglessD3.DOMImpl (runStructure, D3Structure(..)) as D
import TaglessD3.Selection (class AbstractSelection, D3Data(..), append, attrs, d3Select, dataBind, enter, transition)
import TaglessD3.StringImpl (runStructure) as S

d3Script' :: ∀ m. (AbstractSelection m) => m Unit
d3Script' = d3Select "#chart"
         .. dataBind myData
         .. enter
         .. append SvgText
         .. attrs [ CX (V 2.0)] -- , CY (V "45px") ]
         .. transition myTransition

-- myTransition :: D3Transition
myTransition = NamedTransition "t1" $ MS 500

-- myData' :: D3Data Int Number
myData' = ArrayD [1,2,3,4,5] (\i -> divide (toNumber i) 2.0)

-- myData :: D3Data Int Int
myData = ArrayD [1,2,3,4,5] id

main :: forall e. Eff (console :: CONSOLE, d3 :: D3 | e) Unit
main = do
    main2
    logShow $ S.runStructure d3Script' mempty
    log "\n\n\n====== cool beans =======\n\n\n"
    logShow $ D.runStructure d3Script' (initD3Selection :: D.D3Structure Char Char)
