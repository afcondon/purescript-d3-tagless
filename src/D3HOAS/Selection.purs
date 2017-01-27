module D3HOAS.Selection where

import D3HOAS.Base (Attr, D3ElementType, D3Transition, Selector)
import Data.Array (cons)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, Unit, ap, ($), (<>), (<$>), (<<<), map, append)

data D3Structure = D3S (Array String)

data D3Zipper    = Zipper D3S

instance showD3Zipper :: Show D3Zipper where
  show (D3Zipper d3s) = show d3s

data FakeSelection a = FakeSelection (D3Zipper -> (Tuple a D3Zipper))

run :: ∀ a. FakeSelection a -> D3Zipper -> a
run (FakeSelection f) = fst <<< f

instance functorFakeSelection :: Functor FakeSelection where
  map f (FakeSelection a) = FakeSelection $ f a

instance applyFakeSelection :: Apply FakeSelection where
  apply = ap

instance applicativeFakeSelection :: Applicative FakeSelection where
  pure a = FakeSelection a

instance bindFakeSelection :: Bind FakeSelection where
  bind (FakeSelection a) f = f a

instance monadFakeSelection :: Monad FakeSelection

class (Monad d3m) <= Selection d3m where
    d3Select    :: Selector -> d3m String -- starts out as Unit but becomes foreign type (ie actual d3 selection)
    -- d3SelectAll :: Selector -> d3m String
    -- select      :: Selector -> d3m String
    -- selectAll   :: Selector -> d3m String
    merge       :: d3m String -> d3m (Array String)
    -- insert      :: D3ElementType -> d3m String
    -- append      :: D3ElementType -> d3m Unit -> d3m Unit
    -- remove      :: d3m Unit -> Unit
    -- enter       :: d3m Unit -> d3m Unit
    -- exit        :: d3m Unit -> d3m Unit
    -- transition  :: D3Transition -> d3m Unit -> d3m Unit
    -- attrs       :: ∀ d. Array (Attr d) -> d3m Unit -> d3m Unit
    -- dataA       :: ∀ d. Array d -> repr -> repr
    -- dataH       :: ∀ d. Array d -> repr -> repr
    -- dataAI      :: ∀ d i. Array d -> (d -> i) -> repr -> repr
    -- dataHI      :: ∀ d i. Array d -> (d -> i) -> repr -> repr


instance selectionDummySelection :: Selection FakeSelection where
    d3Select selector    = FakeSelection $ d3Select'
    -- d3SelectAll selector = FakeSelection $ "d3SelectAll: " <> selector
    -- select selector      = FakeSelection $ s <> " Select: " <> selector
    -- selectAll selector   = FakeSelection $ s <> " Select: All" <> selector
    merge selection      = FakeSelection $ merge'


d3Select' :: D3Zipper -> (Tuple String D3Zipper)
d3Select' (D3Zipper )
