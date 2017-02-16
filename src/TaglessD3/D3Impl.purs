module D3Impl where

import Prelude
import D3.Selection as D3
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.State (class MonadState)
import Control.Monad.State.Trans (StateT, get, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unit (unit)
import TaglessD3.Base (D3ElementType, D3Transition, Selector)
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
    d3Select selector          = do
        put $ Just $ unsafePerformEff $ D3.d3Select selector
        pure unit

    d3SelectAll selector       = do
        put $ Just $ unsafePerformEff $ D3.d3SelectAll selector
        pure unit

    select selector            = do
        ms <- get
        put $ (unsafePerformEff <<< D3.select selector) <$> ms
        pure unit

    selectAll selector         = do
        ms <- get
        put $ (unsafePerformEff <<< D3.selectAll selector) <$> ms
        pure unit

    merge selection            = do
        ms <- get
        let merging = unsafePerformEff $ fsState selection
        let merged = D3.merge <$> merging <*> ms
        put $ unsafePerformEff <$> merged
        pure unit

    insert element             = do
        ms <- get
        put $ (unsafePerformEff <<< D3.insert (show element)) <$> ms
        pure unit

    append element             = do
        ms <- get
        put $ (unsafePerformEff <<< D3.append (show element)) <$> ms
        pure unit

    remove                     = do
        ms <- get
        put $ (unsafePerformEff <<< D3.remove) <$> ms
        pure unit

    enter                      = do
        ms <- get
        put $ (unsafePerformEff <<< D3.enter) <$> ms
        pure unit

    exit                       = do
        ms <- get
        put $ (unsafePerformEff <<< D3.exit) <$> ms
        pure unit

     -- attrs is complicated by callbacks and also we'd like to hide the repeated call to attr behind array of attr
    attrs attributes           = do -- TODO
        ms <- get
        -- put $ (unsafePerformEff <<< D3.attr attributes) <$> ms
        pure unit

    transition t               = do -- TODO
        ms <- get
        -- put $ (unsafePerformEff <<< D3.selectionTransition t) <$> ms
        pure unit

    dataBind ds = do
        ms <- get
        put $ unsafeCoerce $ case ds of  -- Necessary because Selection value was unknown before this data bind 
                (ArrayD ds (Just k))       -> (unsafePerformEff <<< (D3.dataBindIndexArray ds k)) <$> ms
                (ArrayD ds Nothing)        -> (unsafePerformEff <<< (D3.dataBindArray ds))        <$> ms
                (HierarchyD tree (Just k)) -> (unsafePerformEff <<< (D3.dataBindIndexHierarchy tree k)) <$> ms
                (HierarchyD tree Nothing)  -> (unsafePerformEff <<< (D3.dataBindHierarchy tree))        <$> ms
        pure unit
