module D3HOAS.Selection where

import D3HOAS.Base (Selector)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, Unit, apply, bind, pure, unit, ($), ap)

data FakeSelection a = FakeSelection a -- could have EmptySelection here too?

instance functorFakeSelection :: Functor FakeSelection where
  map f (FakeSelection s) = FakeSelection $ f s

instance applyFakeSelection :: Apply FakeSelection where
  apply = ap

instance applicativeFakeSelection :: Applicative FakeSelection where
  pure a = FakeSelection a

instance bindFakeSelection :: Bind FakeSelection where
  bind (FakeSelection a) f = f a

instance monadFakeSelection :: Monad FakeSelection



class (Monad repr) <= Selection repr where
    d3Select    :: Selector -> repr Unit
    -- d3SelectAll :: Selector -> repr
    -- select      :: Selector -> repr -> repr
    -- selectAll   :: Selector -> repr -> repr
    -- merge       :: repr -> repr -> repr
    -- insert      :: D3ElementType -> repr -> repr
    -- append      :: D3ElementType -> repr -> repr
    -- remove      :: repr -> Unit
    -- enter       :: repr -> repr
    -- exit        :: repr -> repr
    -- transition  :: D3Transition -> repr -> repr
    -- attrs       :: ∀ d. Array (Attr d) -> repr -> repr
    -- dataA       :: ∀ d. Array d -> repr -> repr
    -- dataH       :: ∀ d. Array d -> repr -> repr
    -- dataAI      :: ∀ d i. Array d -> (d -> i) -> repr -> repr
    -- dataHI      :: ∀ d i. Array d -> (d -> i) -> repr -> repr

-- data DummySelection a = DS { d3Selection :: String }
--
-- instance selectionDummySelection :: Selection DummySelection where
--     d3Select selector = DS { d3Selection: selector }
