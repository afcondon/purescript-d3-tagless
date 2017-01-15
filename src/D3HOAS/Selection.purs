module D3HOAS.Selection where

import Prelude (Unit)
import D3HOAS.Base

class Selection repr where
    d3Select    :: Selector -> repr
    -- d3SelectAll :: Selector -> repr
    -- select      :: Selector -> repr -> repr
    -- selectAll   :: Selector -> repr -> repr
    -- merge       :: repr -> repr -> repr
    -- insert      :: D3ElementType -> repr -> repr
    -- append      :: D3ElementType -> repr -> repr
    -- remove      :: repr -> Unit
    -- enter       :: repr -> repr
    -- exit        :: repr -> repr
    -- transition  :: D3Transition -> repr -> repr
    -- attrs       :: ∀ d. Array (Attr d) -> repr -> repr
    -- dataA       :: ∀ d. Array d -> repr -> repr
    -- dataH       :: ∀ d. Array d -> repr -> repr
    -- dataAI      :: ∀ d i. Array d -> (d -> i) -> repr -> repr
    -- dataHI      :: ∀ d i. Array d -> (d -> i) -> repr -> repr

data DummySelection = DS { d3Selection :: String }

instance selectionDummySelection :: Selection DummySelection where
    d3Select selector = DS { d3Selection: selector }
