module TaglessD3.Selection where

import Prelude
import TaglessD3.Base (Attr, D3ElementType, D3Transition, Hierarchy, Selector)

class (Monad m) <= Selection m where
    d3Select    :: Selector -> m Unit -- starts out as Unit but becomes foreign type (ie actual d3 selection)
    d3SelectAll :: Selector -> m Unit
    select      :: Selector -> m Unit
    selectAll   :: Selector -> m Unit
    merge       :: m Unit -> m Unit
    insert      :: D3ElementType -> m Unit
    append      :: D3ElementType -> m Unit
    remove      :: m Unit
    enter       :: m Unit
    exit        :: m Unit
    transition  :: D3Transition -> m Unit
    attrs       :: ∀ d. Array (Attr d) -> m Unit
    dataA       :: ∀ d. Array d -> m (Array d)
    dataH       :: ∀ d. Hierarchy d -> m (Hierarchy d)
    dataAI      :: ∀ d i. Array d -> (d -> i) -> m (Array d)
    dataHI      :: ∀ d i. Hierarchy d -> (d -> i) -> m (Hierarchy d)
