module TaglessD3.DOMImpl where

import TaglessD3.AttrNew
import Control.Monad.Eff (Eff)
import D3.Base (D3)
import D3.Base (D3Selection) as D3
import D3.Selection (Selection, d3Select) as D3
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, class Show, Unit, bind, ap, pure, unit, ($), (<<<), (<>))
import TaglessD3.Base (D3ElementType, D3Transition, Hierarchy, Selector)
import TaglessD3.Selection (class AbstractSelection, D3Data(..))

-- | a very simple Monad to hold the Selection and the data that is bound to it
data D3Structure d i = D3S { selection :: Maybe (D3.Selection d), "data" :: Maybe (D3Data d i) }
type SelectionFn d i a = D3Structure d i -> Tuple a (D3Structure d i)
data RealSelection d i a = RealSelection (SelectionFn d i a)

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

run :: ∀ d i a. RealSelection d i a -> SelectionFn d i a
run (RealSelection f) = f

runData :: ∀ d i a. RealSelection d i a -> D3Structure d i -> a
runData (RealSelection f) = fst <<< f

runStructure :: ∀ d i a. RealSelection d i a -> D3Structure d i -> D3Structure d i
runStructure (RealSelection f) = snd <<< f


instance functorRealSelection :: Functor (RealSelection d i) where
    map f (RealSelection g) = RealSelection $ first f <<< g

instance applyRealSelection :: Apply (RealSelection d i) where
    apply = ap

instance applicativeRealSelection :: Applicative (RealSelection d i) where
    pure a = RealSelection \s -> Tuple a s

instance bindRealSelection :: Bind (RealSelection d i) where
    bind (RealSelection f) k = RealSelection $ \s0 ->
        let as1 = f s0
        in (run $ k $ fst as1) (snd as1)

instance monadRealSelection :: Monad (RealSelection d i)

instance selectionDummySelection :: AbstractSelection (RealSelection d i) where
    d3Select selector          = pure $ RealSelection $ d3Select' selector
    d3SelectAll selector       = d3SelectAll' selector
    select selector            = select' selector
    selectAll selector         = selectAll' selector
    merge selection            = merge' selection
    insert element             = insert' element
    append element             = append' element
    remove                     = remove'
    enter                      = enter'
    exit                       = exit'
    attrs attributes           = attrs' attributes
    transition t               = transition' t
    dataBind (ArrayD ds i)     = dataAI' ds i
    dataBind (HierarchyD ds i) = dataHI' ds i

type D3MonadEff a d i = ∀ e. Eff (d3::D3|e) (Tuple a (D3Structure d i))

-- NB all functions are stubs ATM
-- d3Select' :: ∀ d i e. Selector -> SelectionFn d i Unit
d3Select' selector d3s =
    do
        foo <- D3.d3Select selector
        pure (Tuple unit d3s) -- (updateSelection d3s foo)

d3SelectAll' :: ∀ d i. Selector -> D3Structure d i -> D3MonadEff Unit d i
d3SelectAll' selector d3s = pure (Tuple unit d3s)

select' :: ∀ d i. Selector -> D3Structure d i -> D3MonadEff Unit d i
select' selector d3s = pure (Tuple unit d3s)

selectAll' :: ∀ d i. Selector -> D3Structure d i -> D3MonadEff Unit d i
selectAll' selector d3s = pure (Tuple unit d3s)

merge' :: ∀ d i a. RealSelection d i a -> D3Structure d i -> D3MonadEff String d i
merge' (RealSelection f) d3s = pure (Tuple "merged" d3s)

insert' :: ∀ d i. D3ElementType -> D3Structure d i -> D3MonadEff Unit d i
insert' element d3s = pure (Tuple unit d3s)

append' :: ∀ d i. D3ElementType -> D3Structure d i -> D3MonadEff Unit d i
append' element d3s = pure (Tuple unit d3s)

remove' :: ∀ d i. D3Structure d i -> D3MonadEff Unit d i
remove' d3s = pure (Tuple unit d3s)

enter' :: ∀ d i. D3Structure d i -> D3MonadEff Unit d i
enter' d3s = pure (Tuple unit d3s)

exit' :: ∀ d i. D3Structure d i -> D3MonadEff Unit d i
exit' d3s = pure (Tuple unit d3s)

attrs' :: ∀ d i. List Attr -> D3Structure d i -> D3MonadEff Unit d i
attrs' as d3s = pure (Tuple unit d3s)

transition' :: ∀ d i. D3Transition -> D3Structure d i -> D3MonadEff Unit d i
transition' t d3s = pure (Tuple unit d3s)

dataAI' :: ∀ d d' i i'. Array d' -> (d' -> i') -> D3Structure d i -> D3MonadEff Unit d i
dataAI' ds index d3s = pure (Tuple unit d3s)

dataHI' :: ∀ d d' i i'. Hierarchy d' -> (d' -> i') -> D3Structure d i -> D3MonadEff Unit d i
dataHI' hd index d3s = pure (Tuple unit d3s)
