module D3HOAS.Selection where

import D3HOAS.Base (Attr, D3ElementType, D3Transition, Selector)
import Data.Array (cons)
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, class Show, Unit, ap, append, map, show, unit, ($), (<$>), (<<<), (<>))

data D3Structure = D3S (Array String)

instance showD3Structure :: Show D3Structure where
  show (D3S d3s) = show d3s

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

class (Monad m) <= Selection m where
    d3Select    :: Selector -> m Unit -- starts out as Unit but becomes foreign type (ie actual d3 selection)
    -- d3SelectAll :: Selector -> m String
    -- select      :: Selector -> m String
    -- selectAll   :: Selector -> m String
    merge       :: m Unit -> m Unit
    -- insert      :: D3ElementType -> m String
    -- append      :: D3ElementType -> m String
    -- remove      :: Unit
    -- enter       :: m Unit
    -- exit        :: m Unit
    -- transition  :: D3Transition -> m Unit
    -- attrs       :: ∀ d. Array (Attr d) -> m Unit -> m Unit
    -- dataA       :: ∀ d. Array d -> repr -> repr
    -- dataH       :: ∀ d. Array d -> repr -> repr
    -- dataAI      :: ∀ d i. Array d -> (d -> i) -> repr -> repr
    -- dataHI      :: ∀ d i. Array d -> (d -> i) -> repr -> repr


instance selectionDummySelection :: Selection FakeSelection where
    d3Select selector    = FakeSelection $ d3Select' selector
    -- d3SelectAll selector = FakeSelection $ "d3SelectAll: " <> selector
    -- select selector      = FakeSelection $ s <> " Select: " <> selector
    -- selectAll selector   = FakeSelection $ s <> " Select: All" <> selector
    merge selection      = FakeSelection $ merge' selection


d3Select' :: Selector -> D3Structure -> (Tuple Unit D3Structure)
d3Select' selector (D3S statements) = Tuple unit (D3S $ statements <> ["D3Select", selector])

-- the selection being merged is added to our selection
merge' :: FakeSelection Unit -> D3Structure -> (Tuple Unit D3Structure)
merge' (FakeSelection f) (D3S statements) = Tuple unit (D3S $ statements <> ["D3Merge", "how do we capture the merging selection's name here???"])
