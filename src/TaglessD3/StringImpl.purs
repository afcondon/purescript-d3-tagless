module TaglessD3.StringImpl where

import TaglessD3.AttrNew
import Data.List (List)
import Data.Monoid (class Monoid)
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, class Semigroup, class Show, Unit, ap, pure, show, unit, ($), (<<<), (<>))
import TaglessD3.Base (D3ElementType, D3Transition, Hierarchy, Selector)
import TaglessD3.Selection (class AbstractSelection, D3Data(..))

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

instance selectionDummySelection :: AbstractSelection FakeSelection where
    d3Select selector    = pure $ FakeSelection $ d3Select' selector
    d3SelectAll selector = pure $ FakeSelection $ d3SelectAll' selector
    select selector      = pure $ FakeSelection $ select' selector
    selectAll selector   = pure $ FakeSelection $ selectAll' selector
    merge selection      = pure $ FakeSelection $ merge' selection
    insert element       = pure $ FakeSelection $ insert' element
    append element       = pure $ FakeSelection $ append' element
    remove               = pure $ FakeSelection $ remove'
    enter                = pure $ FakeSelection $ enter'
    exit                 = pure $ FakeSelection $ exit'
    attrs attributes     = pure $ FakeSelection $ attrs' attributes
    transition t         = pure $ FakeSelection $ transition' t
    dataBind (ArrayD ds i)     = pure $ FakeSelection $ dataAI' ds i
    dataBind (HierarchyD ds i) = pure $ FakeSelection $ dataHI' ds i

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
attrs' as d3s = Tuple unit $ d3s ++ [ (renderArrayOfAttributes as) ]

transition' :: D3Transition -> SelectionFn Unit
transition' t d3s = Tuple unit $ d3s ++ [ show t ]

dataAI' :: ∀ d i. Array d -> (d -> i) -> SelectionFn Unit
dataAI' ds index d3s = Tuple unit $ d3s ++ ["Data from Array with index function"]

dataHI' :: ∀ d i. Hierarchy d -> (d -> i) -> SelectionFn Unit
dataHI' hd index d3s = Tuple unit $ d3s ++ ["Hierarchical data with index function"]

-- the selection being merged is added to our selection, but don't yet
-- understand how to capture name from merged selection? maybe change f to
-- different function?
merge' :: FakeSelection Unit -> SelectionFn String
merge' (FakeSelection f) (D3S name statements) = Tuple "merged" (D3S name $ statements <> [["D3Merge", "how do we capture the merging selection's name here???"]])


-- | Utility functions
addD3Statement :: D3Structure -> Array String -> D3Structure
addD3Statement (D3S name statements) statements' = D3S name $ statements <> [statements']

infixl 4 addD3Statement as ++
