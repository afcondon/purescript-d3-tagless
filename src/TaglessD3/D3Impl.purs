module D3Impl where

import Prelude
import D3.Selection as D3
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.State (class MonadState)
import Control.Monad.State.Trans (StateT, get, put, runStateT)
import D3.Base (D3)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, snd)
import TaglessD3.AttrNew (Attr(..), D3Attr(..))
import TaglessD3.Selection (class AbstractSelection, D3Data(..))
import Unsafe.Coerce (unsafeCoerce)

type D3State d = Maybe (D3.Selection d)
newtype D3Monad eff d a = D3Monad (StateT (D3State d) (Eff eff) a)

-- look at all the boilerplate we can save here!
derive newtype instance functorD3Monad     :: Functor           (D3Monad eff d)
derive newtype instance applyD3Monad       :: Apply             (D3Monad eff d)
derive newtype instance applicativeD3Monad :: Applicative       (D3Monad eff d)
derive newtype instance bindD3Monad        :: Bind              (D3Monad eff d)
derive newtype instance monadD3Monad       :: Monad             (D3Monad eff d)
derive newtype instance monadEffD3Monad    :: MonadEff   eff    (D3Monad eff d)
derive newtype instance monadStateD3Monad  :: MonadState (Maybe (D3.Selection d)) (D3Monad eff d)

runD3Monad :: ∀ eff d a. D3Monad eff d a -> Maybe (D3.Selection d) -> Eff eff (Tuple a (D3State d))
runD3Monad (D3Monad m) = runStateT m

fsState :: ∀ a d eff. D3Monad eff d a -> Eff eff (D3State d)
fsState (D3Monad m) = liftA1 snd $ runStateT m Nothing
--
-- fsRun :: ∀ a d eff. D3Monad eff d a -> Eff eff a
-- fsRun (D3Monad m) = liftA1 fst $ runStateT m "initial state"

-- now to fulfill the contract with the abstract definition of D3 Selections
instance selectionDummySelection :: AbstractSelection (D3Monad eff d) where
    d3Select selector = put $ Just $ unsafePerformEff $ D3.d3Select selector

    d3SelectAll selector = put $ Just $ unsafePerformEff $ D3.d3SelectAll selector

    select selector = do
        ms <- get
        put $ (unsafePerformEff <<< D3.select selector) <$> ms

    selectAll selector         = do
        ms <- get
        put $ (unsafePerformEff <<< D3.selectAll selector) <$> ms

    merge selection            = do
        ms <- get
        let merging = unsafePerformEff $ fsState selection
        let merged = D3.merge <$> merging <*> ms
        put $ unsafePerformEff <$> merged

    insert element             = do
        ms <- get
        put $ (unsafePerformEff <<< D3.insert (show element)) <$> ms

    append element             = do
        ms <- get
        put $ (unsafePerformEff <<< D3.append (show element)) <$> ms

    remove                     = do
        ms <- get
        put $ (unsafePerformEff <<< D3.remove) <$> ms

    enter                      = do
        ms <- get
        put $ (unsafePerformEff <<< D3.enter) <$> ms

    exit                       = do
        ms <- get
        put $ (unsafePerformEff <<< D3.exit) <$> ms

    attrs attributes           = do -- TODO
        ms <- get
        put $ (unsafePerformEff <<< D3.listOfAttr attributes) <$> ms

    transition t               = do -- TODO
        ms <- get
        -- put $ (unsafePerformEff <<< D3.selectionTransition t) <$> ms
        pure unit -- TODO

    -- dataBind :: ∀ d i. D3Data d i -> (m Unit)
    dataBind (ArrayD ds (Just k)) = do
        ms <- get
        put $ unsafeCoerce $ (unsafePerformEff <<< (D3.dataBindIndexArray ds k)) <$> ms

    dataBind (ArrayD ds Nothing) = do
        ms <- get
        put $ unsafeCoerce $ (unsafePerformEff <<< (D3.dataBindArray ds)) <$> ms

    dataBind (HierarchyD ds (Just k)) = do
        ms <- get
        put $ unsafeCoerce $ (unsafePerformEff <<< (D3.dataBindIndexHierarchy ds k)) <$> ms

    dataBind (HierarchyD ds Nothing) = do
        ms <- get
        put $ unsafeCoerce $ (unsafePerformEff <<< (D3.dataBindHierarchy ds)) <$> ms

class RunD3 a where
  runD3 :: ∀ e. a -> Eff (d3 :: D3 | e) Unit

instance rund3D3Attr :: RunD3 (D3Attr a) where
  runD3 (D3Attr { value, showValue }) = pure unit
