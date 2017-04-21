module TaglessD3.Selection where

import D3.Base (Hierarchy)
import Data.List (List)
import Prelude (class Monad, Unit)
import TaglessD3.AttrNew (Attr)
import TaglessD3.Base (D3ElementType, D3Transition, Selector)

data D3Data d i = ArrayD     (Array d)
                | HierarchyD (Hierarchy d)
                | ArrayDI     (Array d)     (d -> i)
                | HierarchyDI (Hierarchy d) (d -> i)

class (Monad m) <= AbstractSelection m where
    d3Select    :: Selector      -> (m Unit)
    d3SelectAll :: Selector      -> (m Unit)
    select      :: Selector      -> (m Unit)
    selectAll   :: Selector      -> (m Unit)
    merge       :: m Unit        -> (m Unit)
    insert      :: D3ElementType -> (m Unit)
    append      :: D3ElementType -> (m Unit)
    remove      ::                  (m Unit)
    enter       ::                  (m Unit)
    exit        ::                  (m Unit)
    transition  :: D3Transition  -> (m Unit)
    attrs       :: List Attr     -> (m Unit)
    dataBind    :: ∀ d i. D3Data d i -> (m Unit)
