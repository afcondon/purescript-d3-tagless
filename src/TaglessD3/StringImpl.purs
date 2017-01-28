module TaglessD3.StringImpl where

import Data.Monoid (class Monoid, mempty)
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, class Semigroup, class Show, Unit, ap, show, unit, ($), (<<<), (<>))
import TaglessD3.Base (Attr, D3ElementType, D3Transition, Hierarchy, Selector, renderArrayOfAttributes)
import TaglessD3.Selection (class AbstractSelection)

data D3Structure = D3S String (Array (Array String))

-- initial instance not law-abiding (name' will disappear) but revise to keep
-- list of named selections for merges TODO
instance semigroupD3Structure :: Semigroup D3Structure where
    append (D3S name statements) (D3S name' statements') = D3S name (statements <> statements')

instance monoidD3Structure :: Monoid D3Structure where
    mempty = D3S "" []

instance showD3Structure :: Show D3Structure where
  show (D3S name d3s) = "Selection: " <> name <> "\n\t"<> show d3s

data FakeSelection a = FakeSelection (D3Structure -> Tuple a D3Structure)

run :: ∀ a. FakeSelection a -> D3Structure -> a
run (FakeSelection f) = fst <<< f

run' :: ∀ a. FakeSelection a -> D3Structure -> D3Structure
run' (FakeSelection f) = snd <<< f

instance functorFakeSelection :: Functor FakeSelection where
  map f (FakeSelection g) = FakeSelection $ first f <<< g

instance applyFakeSelection :: Apply FakeSelection where
  apply = ap

instance applicativeFakeSelection :: Applicative FakeSelection where
  pure a = FakeSelection \z -> Tuple a z

instance bindFakeSelection :: Bind FakeSelection where
  bind (FakeSelection f) k = FakeSelection $ \z0 ->
    let az1 = f z0
    in (d3StructureFn $ k $ fst az1) (snd az1)

d3StructureFn :: ∀ a. FakeSelection a -> (D3Structure -> Tuple a D3Structure)
d3StructureFn (FakeSelection f) = f

instance monadFakeSelection :: Monad FakeSelection

instance selectionDummySelection :: AbstractSelection FakeSelection where
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
    transition t         = FakeSelection $ transition' t
    dataA ds             = FakeSelection $ dataA' ds
    dataH hd             = FakeSelection $ dataH' hd
    dataAI ds index      = FakeSelection $ dataAI' ds index
    dataHI hd index      = FakeSelection $ dataHI' hd index

d3Select' :: Selector -> D3Structure -> Tuple Unit D3Structure
d3Select' selector d3s = Tuple unit $ d3s ++ ["D3Select", selector]

d3SelectAll' :: Selector -> D3Structure -> Tuple Unit D3Structure
d3SelectAll' selector d3s = Tuple unit $ d3s ++ ["D3SelectAll", selector]

select' :: Selector -> D3Structure -> Tuple Unit D3Structure
select' selector d3s = Tuple unit $ d3s ++ ["select", selector]

selectAll' :: Selector -> D3Structure -> Tuple Unit D3Structure
selectAll' selector d3s = Tuple unit $ d3s ++ ["selectAll", selector]

insert' :: D3ElementType -> D3Structure -> Tuple Unit D3Structure
insert' element d3s = Tuple unit $ d3s ++ [ "insert", show element ]

append' :: D3ElementType -> D3Structure -> Tuple Unit D3Structure
append' element d3s = Tuple unit $ d3s ++ [ "append", show element ]

remove' :: D3Structure -> Tuple Unit D3Structure
remove' d3s = Tuple unit $ d3s ++ ["Remove"]

enter' :: D3Structure -> Tuple Unit D3Structure
enter' d3s = Tuple unit $ d3s ++ ["Enter"]

exit' :: D3Structure -> Tuple Unit D3Structure
exit' d3s = Tuple unit $ d3s ++ ["Exit"]

attrs' :: ∀ d. Array (Attr d) -> D3Structure -> Tuple Unit D3Structure
attrs' as d3s = Tuple unit $ d3s ++ [ "Attributes: ", renderArrayOfAttributes as ]

transition' :: D3Transition -> D3Structure -> Tuple Unit D3Structure
transition' t d3s = Tuple unit $ d3s ++ [ show t ]

dataA' :: ∀ d. Array d -> D3Structure -> Tuple (Array d) D3Structure
dataA' ds d3s = Tuple ds $ d3s ++ ["Data from Array"]

dataH' :: ∀ d. Hierarchy d -> D3Structure -> Tuple (Hierarchy d) D3Structure
dataH' hd d3s = Tuple hd $ d3s ++ ["Hierarchical data"]

dataAI' :: ∀ d i. Array d -> (d -> i) -> D3Structure -> Tuple (Array d) D3Structure
dataAI' ds index d3s = Tuple ds $ d3s ++ ["Data from Array with index function"]

dataHI' :: ∀ d i. Hierarchy d -> (d -> i) -> D3Structure -> Tuple (Hierarchy d) D3Structure
dataHI' hd index d3s = Tuple hd $ d3s ++ ["Hierarchical data with index function"]

-- the selection being merged is added to our selection, but don't yet
-- understand how to capture name from merged selection? maybe change f to
-- different function?
merge' :: FakeSelection Unit -> D3Structure -> (Tuple Unit D3Structure)
merge' (FakeSelection f) (D3S name statements) = Tuple unit (D3S name $ statements <> [["D3Merge", "how do we capture the merging selection's name here???"]])


-- | Utility functions
addD3Statement :: D3Structure -> Array String -> D3Structure
addD3Statement (D3S name statements) statements' = D3S name $ statements <> [statements']

infixl 4 addD3Statement as ++
