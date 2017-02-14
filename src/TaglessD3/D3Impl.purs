module D3Impl where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.State (class MonadState)
import Control.Monad.State.Trans (StateT, get, put, runStateT)
import Control.Monad.Trans.Class (lift)
import D3.Selection (Selection, d3Select)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst, snd)
import TaglessD3.Base (D3ElementType, D3Transition, Hierarchy, Selector)
import TaglessD3.Selection (class AbstractSelection, D3Data(..))

newtype D3Monad eff d a = D3Monad (StateT (Selection d) (Eff eff) a)

-- look at all the boilerplate we can save here!
derive newtype instance functorD3Monad     :: Functor           (D3Monad eff d)
derive newtype instance applyD3Monad       :: Apply             (D3Monad eff d)
derive newtype instance applicativeD3Monad :: Applicative       (D3Monad eff d)
derive newtype instance bindD3Monad        :: Bind              (D3Monad eff d)
derive newtype instance monadD3Monad       :: Monad             (D3Monad eff d)
derive newtype instance monadEffD3Monad    :: MonadEff   eff    (D3Monad eff d)
derive newtype instance monadStateD3Monad  :: MonadState (Selection d) (D3Monad eff d)

runD3Monad :: ∀ eff d a. D3Monad eff d a -> Selection d -> Eff eff (Tuple a (Selection d))
runD3Monad (D3Monad m) = runStateT m

-- fsState :: ∀ a d eff. D3Monad eff d a -> Eff eff (Selection d)
-- fsState (D3Monad m) = liftA1 snd $ runStateT m "initial state"
--
-- fsRun :: ∀ a d eff. D3Monad eff d a -> Eff eff a
-- fsRun (D3Monad m) = liftA1 fst $ runStateT m "initial state"

-- now to fulfill the contract with the abstract definition of D3 Selections
instance selectionDummySelection :: AbstractSelection (D3Monad eff d) where
    d3Select selector          = do
        put $ unsafePerformEff $ d3Select selector
        pure unit
    d3SelectAll selector       = pure unit -- d3SelectAll' selector
    select selector            = pure unit -- select' selector
    selectAll selector         = pure unit -- selectAll' selector
    merge selection            = pure "unit" -- merge' selection
    insert element             = pure unit -- insert' element
    append element             = pure unit -- append' element
    remove                     = pure unit -- remove'
    enter                      = pure unit -- enter'
    exit                       = pure unit -- exit'
    attrs attributes           = pure unit -- attrs' attributes
    transition t               = pure unit -- transition' t
    dataBind (ArrayD ds i)     = pure unit -- dataAI' ds i
    dataBind (HierarchyD ds i) = pure unit -- dataHI' ds i

d3SelectAll' selector d3s = pure unit

select' selector d3s = pure unit

selectAll' selector d3s = pure unit

merge' (D3Monad f) d3s = pure (Tuple "merged" d3s)

insert' element d3s = pure unit

append' element d3s = pure unit

remove' d3s = pure unit

enter' d3s = pure unit

exit' d3s = pure unit

attrs' as d3s = pure unit

transition' t d3s = pure unit

dataAI' ds index d3s = pure unit

dataHI' hd index d3s = pure unit
