module TaglessD3.DOMImpl where

import D3.Base (D3Selection) as D3
import D3.Selection (Selection, d3Select) as D3
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, class Show, Unit, ap, unit, ($), (<<<), (<>))
import TaglessD3.Base (Attr, D3ElementType, D3Transition, Hierarchy, Selector)
import TaglessD3.Selection (class AbstractSelection, D3Data(..))

-- | a very simple Monad to hold the Selection and the data that is bound to it
data D3Structure d i = D3S { selection :: Maybe (D3.Selection d), "data" :: Maybe (D3Data d i) }
data D3Monad d i a = D3Monad (D3Structure d i -> Tuple a (D3Structure d i))

initD3Selection :: ∀ d i. D3Structure d i
initD3Selection = D3S { selection: Nothing, "data": Nothing }

instance showD3Structure :: Show (D3Structure d i) where
  show (D3S { selection: s, "data": d }) = "Selection: " <> ss <> " " <> ds where
    ss = case s of
         Just _  -> "Has a selection"
         Nothing -> "Unitialized selection"
    ds = case d of
         Just (ArrayD _ _)      -> "Data: Array"
         Just (HierarchyD _ _)  -> "Data: Hierarchical"
         Nothing -> "No data has been bound"

run :: ∀ d i a. D3Monad d i a -> D3Structure d i -> Tuple a (D3Structure d i)
run (D3Monad f) = f

runData :: ∀ d i a. D3Monad d i a -> D3Structure d i -> a
runData (D3Monad f) = fst <<< f

runStructure :: ∀ d i a. D3Monad d i a -> D3Structure d i -> D3Structure d i
runStructure (D3Monad f) = snd <<< f


instance functorD3Monad :: Functor (D3Monad d i) where
    map f (D3Monad g) = D3Monad $ first f <<< g

instance applyD3Monad :: Apply (D3Monad d i) where
    apply = ap

instance applicativeD3Monad :: Applicative (D3Monad d i) where
    pure a = D3Monad \s -> Tuple a s

instance bindD3Monad :: Bind (D3Monad d i) where
    bind (D3Monad f) k = D3Monad $ \s0 ->
        let as1 = f s0
        in (d3StructureFn $ k $ fst as1) (snd as1)

d3StructureFn :: ∀ i a d. D3Monad d i a -> (D3Structure d i -> Tuple a (D3Structure d i))
d3StructureFn (D3Monad f) = f

instance monadD3Monad :: Monad (D3Monad d i)

instance selectionDummySelection :: AbstractSelection (D3Monad d i) where
    d3Select selector          = D3Monad $ d3Select' selector
    d3SelectAll selector       = D3Monad $ d3SelectAll' selector
    select selector            = D3Monad $ select' selector
    selectAll selector         = D3Monad $ selectAll' selector
    merge selection            = D3Monad $ merge' selection
    insert element             = D3Monad $ insert' element
    append element             = D3Monad $ append' element
    remove                     = D3Monad $ remove'
    enter                      = D3Monad $ enter'
    exit                       = D3Monad $ exit'
    attrs attributes           = D3Monad $ attrs' attributes
    transition t               = D3Monad $ transition' t
    dataBind (ArrayD ds i)     = D3Monad $ dataAI' ds i
    dataBind (HierarchyD ds i) = D3Monad $ dataHI' ds i

-- NB all functions are stubs ATM
d3Select' :: ∀ d i. Selector -> D3Structure d i -> Tuple Unit (D3Structure d i)
d3Select' selector d3s = Tuple unit d3s -- but what we'd really like to do is in comment
    -- do
    --     foo <- D3.d3Select selector
    --     Tuple unit (updateSelection d3s foo)

d3SelectAll' :: ∀ d i. Selector -> D3Structure d i -> Tuple Unit (D3Structure d i)
d3SelectAll' selector d3s = Tuple unit d3s

select' :: ∀ d i. Selector -> D3Structure d i -> Tuple Unit (D3Structure d i)
select' selector d3s = Tuple unit d3s

selectAll' :: ∀ d i. Selector -> D3Structure d i -> Tuple Unit (D3Structure d i)
selectAll' selector d3s = Tuple unit d3s

merge' :: ∀ d i a. D3Monad d i a -> D3Structure d i -> (Tuple String (D3Structure d i))
merge' (D3Monad f) d3s = Tuple "merged" d3s

insert' :: ∀ d i. D3ElementType -> D3Structure d i -> Tuple Unit (D3Structure d i)
insert' element d3s = Tuple unit d3s

append' :: ∀ d i. D3ElementType -> D3Structure d i -> Tuple Unit (D3Structure d i)
append' element d3s = Tuple unit d3s

remove' :: ∀ d i. D3Structure d i -> Tuple Unit (D3Structure d i)
remove' d3s = Tuple unit d3s

enter' :: ∀ d i. D3Structure d i -> Tuple Unit (D3Structure d i)
enter' d3s = Tuple unit d3s

exit' :: ∀ d i. D3Structure d i -> Tuple Unit (D3Structure d i)
exit' d3s = Tuple unit d3s

attrs' :: ∀ d i a. Array (Attr a) -> D3Structure d i -> Tuple Unit (D3Structure d i)
attrs' as d3s = Tuple unit d3s

transition' :: ∀ d i. D3Transition -> D3Structure d i -> Tuple Unit (D3Structure d i)
transition' t d3s = Tuple unit d3s

dataAI' :: ∀ d d' i i'. Array d' -> (d' -> i') -> D3Structure d i -> Tuple Unit (D3Structure d i)
dataAI' ds index d3s = Tuple unit d3s

dataHI' :: ∀ d d' i i'. Hierarchy d' -> (d' -> i') -> D3Structure d i -> Tuple Unit (D3Structure d i)
dataHI' hd index d3s = Tuple unit d3s
