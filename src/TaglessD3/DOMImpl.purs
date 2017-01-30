module TaglessD3.DOMImpl where

import D3.Base (D3Selection) as D3
import D3.Selection (Selection, d3Select) as D3
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, class Show, Unit, ap, show, unit, ($), (<<<), (<>), (<$>))
import TaglessD3.Base (Attr, D3ElementType, D3Transition, Hierarchy, Selector, renderArrayOfAttributes)
import TaglessD3.Selection (class AbstractSelection)

data D3Data = AI (Array Int)
            | AN (Array Number)
            | AS (Array String)
            | AC (Array Char)
            | HI (Hierarchy Int)
            | HN (Hierarchy Number)
            | HS (Hierarchy String)
            | HC (Hierarchy Char)

-- | a very simple Monad to hold the Selection and the data that is bound to it
data D3Monad d a = D3Monad (Maybe (D3.Selection d)) (Maybe D3Data) a

instance functorD3Monad :: Functor (D3Monad d) where
    map f (D3Monad s d x) = D3Monad s d (f x)

instance applyD3Monad :: Apply (D3Monad d) where
    apply (D3Monad s d fn) x = fn <$> x

instance applicativeD3Monad :: Applicative (D3Monad d) where
    pure d = D3Monad Nothing Nothing d

instance bindD3Monad :: Bind (D3Monad d) where
    bind d3m@(D3Monad s d x) k = concat d3m $ k x
        where
        concat :: ∀  s d s' d'. D3Monad s d -> D3Monad s' d' -> D3Monad s d'
        concat (D3Monad s d x) (D3Monad s' d' y) = (D3Monad s d y)


instance monadD3Monad :: Monad (D3Monad d)

instance selectionDummySelection :: AbstractSelection (D3Monad d) where
    d3Select selector    = D3Monad (d3Select'    selector) Nothing unit
    d3SelectAll selector = D3Monad (d3SelectAll' selector) Nothing unit
    select selector      = D3Monad (select'      selector) Nothing unit
    selectAll selector   = D3Monad (selectAll'   selector) Nothing unit
    merge selection      = D3Monad
    insert element       = D3Monad
    append element       = D3Monad
    remove               = D3Monad
    enter                = D3Monad
    exit                 = D3Monad
    attrs attributes     = D3Monad
    transition t         = D3Monad
    dataA ds             = D3Monad
    dataH hd             = D3Monad
    dataAI ds index      = D3Monad
    dataHI hd index      = D3Monad

d3Select' :: ∀ d. Selector -> Maybe (D3.Selection d)
d3Select' _ = Nothing

-- d3Select' :: ∀ d. Selector -> D3DOMStructure d -> Tuple Unit (D3DOMStructure d)
-- d3Select' selector d3s = Tuple unit (D3S (d3Select selector))
--
-- d3SelectAll' :: ∀ d. Selector -> D3DOMStructure d -> Tuple Unit (D3DOMStructure d)
-- d3SelectAll' selector d3s = Tuple unit d3s
--
-- select' :: ∀ d. Selector -> D3DOMStructure d -> Tuple Unit (D3DOMStructure d)
-- select' selector d3s = Tuple unit d3s
--
-- selectAll' :: ∀ d. Selector -> D3DOMStructure d -> Tuple Unit (D3DOMStructure d)
-- selectAll' selector d3s = Tuple unit d3s
--
-- insert' :: ∀ d. D3ElementType -> D3DOMStructure d -> Tuple Unit (D3DOMStructure d)
-- insert' element d3s = Tuple unit d3s
--
-- append' :: ∀ d. D3ElementType -> D3DOMStructure d -> Tuple Unit (D3DOMStructure d)
-- append' element d3s = Tuple unit d3s
--
-- remove' :: ∀ d. D3DOMStructure d -> Tuple Unit (D3DOMStructure d)
-- remove' d3s = Tuple unit d3s
--
-- enter' :: ∀ d. D3DOMStructure d -> Tuple Unit (D3DOMStructure d)
-- enter' d3s = Tuple unit d3s
--
-- exit' :: ∀ d. D3DOMStructure d -> Tuple Unit (D3DOMStructure d)
-- exit' d3s = Tuple unit d3s
--
-- attrs' :: ∀ d. Array (Attr d) -> D3DOMStructure d -> Tuple Unit (D3DOMStructure d)
-- attrs' as d3s = Tuple unit d3s
--
-- transition' :: ∀ d. D3Transition -> D3DOMStructure d -> Tuple Unit (D3DOMStructure d)
-- transition' t d3s = Tuple unit d3s
--
-- dataA' :: ∀ d. Array d -> D3DOMStructure d -> Tuple (Array d) (D3DOMStructure d)
-- dataA' ds d3s = Tuple ds d3s
--
-- dataH' :: ∀ d. Hierarchy d -> D3DOMStructure d -> Tuple (Hierarchy d) (D3DOMStructure d)
-- dataH' hd d3s = Tuple hd d3s
--
-- dataAI' :: ∀ d i. Array d -> (d -> i) -> D3DOMStructure d -> Tuple (Array d) (D3DOMStructure d)
-- dataAI' ds index d3s = Tuple ds d3s
--
-- dataHI' :: ∀ d i. Hierarchy d -> (d -> i) -> D3DOMStructure d -> Tuple (Hierarchy d) (D3DOMStructure d)
-- dataHI' hd index d3s = Tuple hd d3s
--
-- merge' :: ∀ d. D3Wrapper Unit -> D3DOMStructure d -> (Tuple Unit (D3DOMStructure d))
-- merge' (D3Wrapper f) (D3S name selection) = Tuple unit (D3S name selection)
--
