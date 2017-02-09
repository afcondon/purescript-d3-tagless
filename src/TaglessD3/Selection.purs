module TaglessD3.Selection where

import Prelude (class Monad, Unit)
import TaglessD3.AttrNew (Attr)
import Control.Monad.Eff (Eff)
import D3.Base (D3)
import Data.List (List)
import TaglessD3.Base (D3ElementType, D3Transition, Hierarchy, Selector)

data D3Data d i = ArrayD (Array d) (d -> i)
                | HierarchyD (Hierarchy d) (d -> i)

class (Monad m) <= AbstractSelection m where
    d3Select    :: Selector      -> Eff (d3::D3) (m Unit)
    d3SelectAll :: Selector      -> Eff (d3::D3) (m Unit)
    select      :: Selector      -> Eff (d3::D3) (m Unit)
    selectAll   :: Selector      -> Eff (d3::D3) (m Unit)
    merge       :: m Unit        -> Eff (d3::D3) (m String)
    insert      :: D3ElementType -> Eff (d3::D3) (m Unit)
    append      :: D3ElementType -> Eff (d3::D3) (m Unit)
    remove      ::                  Eff (d3::D3) (m Unit)
    enter       ::                  Eff (d3::D3) (m Unit)
    exit        ::                  Eff (d3::D3) (m Unit)
    transition  :: D3Transition  -> Eff (d3::D3) (m Unit)
    attrs       :: List Attr     -> Eff (d3::D3) (m Unit)
    dataBind    :: ∀ d i. D3Data d i -> Eff (d3::D3) (m Unit)
