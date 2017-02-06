module TaglessD3.Selection where

import Prelude
import TaglessD3.Base (D3ElementType, D3Transition, Hierarchy, Selector)
import TaglessD3.Attr

data D3Data d i = ArrayD (Array d) (d -> i)
                | HierarchyD (Hierarchy d) (d -> i)

class (Monad m) <= AbstractSelection m where
    d3Select    :: Selector -> m Unit -- starts out as Unit but becomes foreign type (ie actual d3 selection)
    d3SelectAll :: Selector -> m Unit
    select      :: Selector -> m Unit
    selectAll   :: Selector -> m Unit
    merge       :: m Unit -> m String
    insert      :: D3ElementType -> m Unit
    append      :: D3ElementType -> m Unit
    remove      :: m Unit
    enter       :: m Unit
    exit        :: m Unit
    transition  :: D3Transition -> m Unit
    attrs       :: ∀ d. Array (Attr d) -> m Unit
    dataBind    :: ∀ d i. D3Data d i -> m Unit
