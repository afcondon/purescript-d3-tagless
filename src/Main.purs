module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Monoid (mempty)
import Prelude (Unit, ($), bind)
import TaglessD3.Base (Attr(..), D3ElementType(..), D3Transition(..), Duration(..), ValueOrCallback(..), (..))
import TaglessD3.DOMImpl (run', D3DOMStructure(..)) as D
import TaglessD3.Selection (class AbstractSelection, append, attrs, d3Select, dataA, enter, transition)
import TaglessD3.StringImpl (run') as S

d3Script' :: ∀ m. (AbstractSelection m) => m Unit
d3Script' = d3Select "#chart"
         .. dataA [1,2,3,4,5]
         .. enter
         .. append SvgText
         .. attrs [ CX (V 2.0)] -- , CY (V "45px") ]
         .. transition (NamedTransition "t1" (MS 500))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
    logShow $ S.run' d3Script' mempty
    log "\n\n\ncool beans\n\n\n"
    logShow $ D.run' d3Script' (D.D3S "dom")
