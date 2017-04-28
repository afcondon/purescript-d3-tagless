module TaglessD3.StringImpl where

import D3.Transition (D3Transition(..), TimeSpec(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, class Semigroup, class Show, Unit, ap, show, unit, ($), (<<<), (<>))
import TaglessD3.API (class AbstractD3API, D3Data(..), tDelay)
import TaglessD3.AttrNew (Attr, showListOfAttributes)
import TaglessD3.Base (D3ElementType, Selector)

data D3Structure = D3S String (Array (Array String))

-- initial instance not law-abiding (name' will disappear) but revise to keep
-- list of named selections for merges TODO
instance semigroupD3Structure :: Semigroup D3Structure where
    append (D3S name statements) (D3S name' statements') = D3S name (statements <> statements')

instance monoidD3Structure :: Monoid D3Structure where
    mempty = D3S "" []

instance showD3Structure :: Show D3Structure where
  show (D3S name d3s) = "Selection: " <> name <> "\n\t"<> show d3s

type SelectionFn a = (D3Structure -> Tuple a D3Structure)
data FakeSelection a = FakeSelection (SelectionFn a)

run :: ∀ a. FakeSelection a -> SelectionFn a
run (FakeSelection f) = f

runData :: ∀ a. FakeSelection a -> D3Structure -> a
runData (FakeSelection f) = fst <<< f

runStructure :: ∀ a. FakeSelection a -> D3Structure -> D3Structure
runStructure (FakeSelection f) = snd <<< f

instance functorFakeSelection :: Functor FakeSelection where
  map f (FakeSelection g) = FakeSelection $ first f <<< g

instance applyFakeSelection :: Apply FakeSelection where
  apply = ap

instance applicativeFakeSelection :: Applicative FakeSelection where
  pure a = FakeSelection \z -> Tuple a z

instance bindFakeSelection :: Bind FakeSelection where
  bind (FakeSelection f) k = FakeSelection $ \z0 ->
    let az1 = f z0
    in (run $ k $ fst az1) (snd az1) -- fst & snd can go when we have pattern match in let

instance monadFakeSelection :: Monad FakeSelection

instance selectionDummySelection :: AbstractD3API FakeSelection where
    d3Select selector    = FakeSelection $ d3Select' selector
    d3SelectAll selector = FakeSelection $ d3SelectAll' selector
    select selector      = FakeSelection $ select' selector
    selectAll selector   = FakeSelection $ selectAll' selector
    merge selection      = FakeSelection $ merge' selection
    insert element       = FakeSelection $ insert' element
    append element       = FakeSelection $ append' element
    remove               = FakeSelection $ remove'
    enter                = FakeSelection $ enter'
    exit                 = FakeSelection $ exit'
    attrs attributes     = FakeSelection $ attrs' attributes
    applyTransition t    = FakeSelection $ applyTransition' t
    dataBind d           = FakeSelection $ dataBind d
    tAttrs attributes    = FakeSelection $ tAttrs' attributes
    tMerge selection     = FakeSelection $ tMerge' selection
    tRemove              = FakeSelection $ tRemove'
    tSelect selector     = FakeSelection $ tSelect' selector
    tSelectAll selector  = FakeSelection $ tSelectAll' selector
    makeTransition t     = FakeSelection $ makeTransition' t
    tDelay t             = FakeSelection $ tDelay' t
    tDuration t             = FakeSelection $ tDuration' t

d3Select' :: Selector -> SelectionFn Unit
d3Select' selector d3s = Tuple unit $ d3s ++ ["D3Select", selector]

d3SelectAll' :: Selector -> SelectionFn Unit
d3SelectAll' selector d3s = Tuple unit $ d3s ++ ["D3SelectAll", selector]

select' :: Selector -> SelectionFn Unit
select' selector d3s = Tuple unit $ d3s ++ ["select", selector]

selectAll' :: Selector -> SelectionFn Unit
selectAll' selector d3s = Tuple unit $ d3s ++ ["selectAll", selector]

insert' :: D3ElementType -> SelectionFn Unit
insert' element d3s = Tuple unit $ d3s ++ [ "insert", show element ]

append' :: D3ElementType -> SelectionFn Unit
append' element d3s = Tuple unit $ d3s ++ [ "append", show element ]

remove' :: SelectionFn Unit
remove' d3s = Tuple unit $ d3s ++ ["Remove"]

enter' :: SelectionFn Unit
enter' d3s = Tuple unit $ d3s ++ ["Enter"]

exit' :: SelectionFn Unit
exit' d3s = Tuple unit $ d3s ++ ["Exit"]

attrs' :: List Attr -> SelectionFn Unit
attrs' as d3s = Tuple unit $ d3s ++ [ (showListOfAttributes as) ]

applyTransition' :: FakeSelection Unit -> SelectionFn Unit -- essentially the same as a merge
applyTransition' _ d3s = Tuple unit $ d3s ++ [ "transition", "name" ] -- got to get the name and details out of the transition here

dataBind    :: ∀ d i. D3Data d i -> SelectionFn Unit
dataBind (ArrayDI     ds k) d3s = Tuple unit $ d3s ++ ["Data from Array with index function"]
dataBind (ArrayD     ds)  d3s   = Tuple unit $ d3s ++ ["Data from Array (no index fn)"]
dataBind (HierarchyDI hs k) d3s = Tuple unit $ d3s ++ ["Data from Hierarchy with index function"]
dataBind (HierarchyD ds)  d3s   = Tuple unit $ d3s ++ ["Data from Hierarchy (no index fn)"]

-- the selection being merged is added to our selection, but don't yet
-- understand how to capture name from merged selection? maybe change f to
-- different function?
merge' :: FakeSelection Unit -> SelectionFn Unit
merge' (FakeSelection f) (D3S name statements) = Tuple unit (D3S name $ statements <> [["D3Merge", "how do we capture the merging selection's name here???"]])

tAttrs' :: List Attr -> SelectionFn Unit
tAttrs' as d3s    = Tuple unit $ d3s ++ [(showListOfAttributes as)]

tMerge' :: FakeSelection Unit -> SelectionFn Unit
tMerge' (FakeSelection f) (D3S name statements) = Tuple unit (D3S name $ statements <> [["D3 Transition Merge", "how do we capture the merging selection's name here???"]])

tRemove' :: SelectionFn Unit
tRemove' d3s = Tuple unit $ d3s ++ ["tRemove"]

tSelect' :: Selector -> SelectionFn Unit
tSelect' selector d3s = Tuple unit $ d3s ++ ["tSelect", selector]

tSelectAll' :: Selector -> SelectionFn Unit
tSelectAll' selector d3s  = Tuple unit $ d3s ++ ["tSelectAll", selector]

tDelay' :: ∀ d. TimeSpec d -> SelectionFn Unit
tDelay' (MilliSec t) d3s = Tuple unit $ d3s ++ ["Delay in ms: ", show t]
tDelay' (DelayFn f) d3s = Tuple unit $ d3s ++ ["Delay provided as function"]

tDuration' :: ∀ d. TimeSpec d -> SelectionFn Unit
tDuration' (MilliSec t) d3s = Tuple unit $ d3s ++ ["Duration in ms: ", show t]
tDuration' (DelayFn f) d3s = Tuple unit $ d3s ++ ["Duration provided as function"]

makeTransition' :: D3Transition  -> SelectionFn Unit
makeTransition' (TransitionName tn) d3s = Tuple unit $ d3s ++ ["Transition: ", tn]
makeTransition' UnnamedTransition d3s = Tuple unit $ d3s ++ ["Unnamed ransition"]


-- | Utility functions
addD3Statement :: D3Structure -> Array String -> D3Structure
addD3Statement (D3S name statements) statements' = D3S name $ statements <> [statements']

infixl 4 addD3Statement as ++
