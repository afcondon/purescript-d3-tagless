module TaglessD3.D3Impl where

import Effect
import Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.State.Trans (StateT, get, put, runStateT)
import D3.Selection as D3
import D3.Transition as D3
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, snd)
import Effect.Unsafe (unsafePerformEffect)
import TaglessD3.API (class AbstractD3API, D3Data(..))
import TaglessD3.AttrNew (D3Attr(D3Attr))
import Unsafe.Coerce (unsafeCoerce)

type D3Script     = ∀ m. (AbstractD3API m) => m Unit

type D3State d = Maybe (D3.Selection d)
-- StateT :: forall a m s. (s -> m (Tuple a s)) -> StateT s m a
-- newtype D3Monad eff d a = D3Monad (StateT (D3State d) (Eff eff) a)
newtype D3Monad d a = D3Monad (StateT (D3State d) Effect a)

-- look at all the boilerplate we can save here!
derive newtype instance functorD3Monad     :: Functor           (D3Monad d)
derive newtype instance applyD3Monad       :: Apply             (D3Monad d)
derive newtype instance applicativeD3Monad :: Applicative       (D3Monad d)
derive newtype instance bindD3Monad        :: Bind              (D3Monad d)
derive newtype instance monadD3Monad       :: Monad             (D3Monad d)
derive newtype instance monadStateD3Monad  :: MonadState (Maybe (D3.Selection d)) (D3Monad d)

runD3Monad :: ∀ d a. D3Monad d a -> Maybe (D3.Selection d) -> Effect (Tuple a (D3State d))
runD3Monad (D3Monad m) = runStateT m

fsState :: ∀ a d. D3Monad d a -> Effect (D3State d)
fsState (D3Monad m) = liftA1 snd $ runStateT m Nothing
--
-- fsRun :: ∀ a d eff. D3Monad eff d a -> Eff eff a
-- fsRun (D3Monad m) = liftA1 fst $ runStateT m "initial state"

-- now to fulfill the contract with the abstract definition of D3 Selections
instance selectionDummySelection :: AbstractD3API (D3Monad d) where
    d3Select selector = put $ Just $ unsafePerformEffect $ D3.d3Select selector

    d3SelectAll selector = put $ Just $ unsafePerformEffect $ D3.d3SelectAll selector

    select selector = do
        ms <- get
        put $ (unsafePerformEffect <<< D3.select selector) <$> ms

    selectAll selector         = do
        ms <- get
        put $ (unsafePerformEffect <<< D3.selectAll selector) <$> ms

    merge selection            = do
        ms <- get
        let merging = unsafePerformEffect $ fsState selection
        let merged = D3.merge <$> merging <*> ms
        put $ unsafePerformEffect <$> merged

    insert element             = do
        ms <- get
        put $ (unsafePerformEffect <<< D3.insert (show element)) <$> ms

    append element             = do
        ms <- get
        put $ (unsafePerformEffect <<< D3.append (show element)) <$> ms

    remove                     = do
        ms <- get
        put $ (unsafePerformEffect <<< D3.remove) <$> ms

    enter                      = do
        ms <- get
        put $ (unsafePerformEffect <<< D3.enter) <$> ms

    exit                       = do
        ms <- get
        put $ (unsafePerformEffect <<< D3.exit) <$> ms

    attrs attributes           = do
        ms <- get
        put $ (unsafePerformEffect <<< D3.listOfAttr attributes) <$> ms

    applyTransition t               = do
        ms <- get
        let tAsMonad = unsafePerformEffect $ fsState t
        let transitionApplied = D3.transition <$> tAsMonad <*> ms
        put $ unsafePerformEffect <$> transitionApplied
-- in API.purs
    -- applyTransition  :: (m Unit)  -> (m Unit)
-- in D3.Transition.purs
    -- d3Transition :: ∀ eff d. D3Transition -> Effect (Selection d)
    -- addTransition :: ∀ d eff.  String -> Selection d  -> Effect (Selection d)

    -- dataBind :: ∀ d i. D3Data d i -> (m Unit)
    dataBind (ArrayDI ds k) = do
        ms <- get
        put $ unsafeCoerce $ (unsafePerformEffect <<< (D3.dataBindIndexArray ds k)) <$> ms

    dataBind (ArrayD ds) = do
        ms <- get
        put $ unsafeCoerce $ (unsafePerformEffect <<< (D3.dataBindArray ds)) <$> ms

    dataBind (HierarchyDI ds k) = do
        ms <- get
        put $ unsafeCoerce $ (unsafePerformEffect <<< (D3.dataBindIndexHierarchy ds k)) <$> ms

    dataBind (HierarchyD ds) = do
        ms <- get
        put $ unsafeCoerce $ (unsafePerformEffect <<< (D3.dataBindHierarchy ds)) <$> ms

    -- tMerge       :: m Unit        -> (m Unit)
    tMerge selection            = do
        ms <- get
        let merging = unsafePerformEffect $ fsState selection
        let merged = D3.merge <$> merging <*> ms
        put $ unsafePerformEffect <$> merged

    -- tRemove      ::                  (m Unit)
    tRemove                     = do
        ms <- get
        put $ (unsafePerformEffect <<< D3.remove) <$> ms

    -- tSelect      :: Selector      -> (m Unit)
    tSelect selector = do
        ms <- get
        put $ (unsafePerformEffect <<< D3.select selector) <$> ms

    -- tSelectAll   :: Selector      -> (m Unit)
    tSelectAll selector         = do
        ms <- get
        put $ (unsafePerformEffect <<< D3.selectAll selector) <$> ms

    -- tTransition  :: D3Transition  -> (m Unit)
    makeTransition t = put $ Just $ unsafePerformEffect $ D3.d3Transition t

    delay t = do
        ms <- get
        let t' = unsafeCoerce t -- is this coerce still needed?? TODO
        put $ (unsafePerformEffect <<< D3.delay t') <$> ms

    -- tDuration       :: ∀ d. TimeSpec d -> (m Unit)
    duration t = do
        ms <- get
        let t' = unsafeCoerce t  -- is this coerce still needed?? TODO
        put $ (unsafePerformEffect <<< D3.duration t') <$> ms



class RunD3 a where
  runD3 :: ∀ e. a -> Effect Unit

instance rund3D3Attr :: RunD3 (D3Attr a) where
  runD3 (D3Attr { value, showValue }) = pure unit
