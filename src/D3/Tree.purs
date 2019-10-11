module D3.Tree where

import Prelude
import Effect
import Effect.Uncurried (mkEffectFn2, EffectFn1, runEffectFn1, runEffectFn2, EffectFn2)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe, Nullable)


foreign import data D3Tree           :: Type
foreign import data D3Hierarchy      :: Type

type HierarchyLink d = { source :: (HierarchyNode d), target :: (HierarchyNode d) }
type HierarchyNode d = {
    data     :: d                    -- the associated data, as specified to the constructor
  , depth    :: Number               -- zero for the root node, and increasing by one for each descendant generation
  , height   :: Number               -- zero for leaf nodes, and the greatest distance from any descendant leaf for internal nodes
  -- , parent   :: (HierarchyNode d)        -- the parent node, or null for the root node
  -- , children :: Array (HierarchyNode d)  -- an array of child nodes, if any; undefined for leaf nodes.
  , value    :: Number               -- the summed value of the node and its descendants; optional, set by node.sum.
  , x        :: Number
  , y        :: Number
}

foreign import d3HierarchyFn :: forall d. EffectFn1 (Array d)         (HierarchyNode d)
foreign import d3TreeFn      :: Effect                                 D3Tree
foreign import sizeFn        :: EffectFn2 (Array Number) D3Tree        D3Tree
foreign import nodeSizeFn    :: EffectFn2 (Array Number) D3Tree        D3Tree
foreign import separationFn  :: forall d. EffectFn2
                                          (EffectFn2 (HierarchyNode d) (HierarchyNode d) Number)
                                          D3Tree
                                          D3Tree
foreign import hierarchizeFn :: forall d. EffectFn2 (Array d) D3Hierarchy        (HierarchyNode d)
foreign import treeFn        :: forall d. EffectFn2 (HierarchyNode d) D3Tree     (HierarchyNode d)

foreign import hasChildrenFn :: forall d. EffectFn1 (HierarchyNode d)            Boolean
foreign import ancestorsFn   :: forall d. EffectFn1 (HierarchyNode d)            (Array (HierarchyNode d))
foreign import descendantsFn :: forall d. EffectFn1 (HierarchyNode d)            (Array (HierarchyNode d))
-- foreign import eachFn        :: forall d. EffectFn1 (HierarchyNode d)
-- foreign import eachAfterFn   :: forall d. EffectFn1 (HierarchyNode d)
-- foreign import eachBeforeFn  :: forall d. EffectFn1 (HierarchyNode d)
foreign import leavesFn      :: forall d. EffectFn1 (HierarchyNode d)            (Array (HierarchyNode d))
foreign import linksFn       :: forall d. EffectFn1 (HierarchyNode d)            (Array (HierarchyLink d))
-- foreign import pathFn        :: forall d. EffectFn1 (HierarchyNode d)
-- foreign import sortFn        :: forall d. EffectFn1 (HierarchyNode d)
foreign import sumFn         :: forall d. EffectFn1 (HierarchyNode d)                              Number
foreign import childrenFn    :: forall d. EffectFn1 (HierarchyNode d)            (Array (HierarchyNode d))
foreign import parentFn      :: forall d. EffectFn1 (HierarchyNode d)         (Nullable (HierarchyNode d))
foreign import parentsEqFn   :: ∀ d. HierarchyNode d -> HierarchyNode d -> Boolean

-- no version yet to take the children accessor function
d3Hierarchy :: forall d. Array d -> Effect (HierarchyNode d)
d3Hierarchy = runEffectFn1 d3HierarchyFn

d3Tree :: Effect D3Tree
d3Tree = d3TreeFn

hierarchize :: forall d. Array d -> D3Hierarchy -> Effect (HierarchyNode d)
hierarchize = runEffectFn2 hierarchizeFn

size :: Number -> Number -> D3Tree -> Effect D3Tree
size width height = runEffectFn2 sizeFn [width, height]

nodeSize :: Number -> Number -> D3Tree -> Effect D3Tree
nodeSize width height = runEffectFn2 nodeSizeFn [width, height]

-- || functions on the Tree Layout

-- d3.tree()()    tree as function, lays out HierarchyNodes as a Tree
layoutTree :: forall d. HierarchyNode d -> D3Tree -> Effect (HierarchyNode d)
layoutTree = runEffectFn2 treeFn

-- || functions on HierarchyNodes
ancestors   :: forall d. HierarchyNode d   -> Effect (Array (HierarchyNode d))
ancestors   = runEffectFn1 ancestorsFn

descendants :: forall d. HierarchyNode d -> Effect (Array (HierarchyNode d))
descendants  = runEffectFn1 descendantsFn

leaves      :: forall d. HierarchyNode d  -> Effect (Array (HierarchyNode d))
leaves      = runEffectFn1 leavesFn

links       :: forall d. HierarchyNode d -> Effect (Array (HierarchyLink d))
links       = runEffectFn1 linksFn

sum         :: forall d. HierarchyNode d -> Effect Number
sum         = runEffectFn1 sumFn

separation  :: forall d. (HierarchyNode d -> HierarchyNode d -> Effect Number) -> D3Tree -> Effect D3Tree
separation f = runEffectFn2 separationFn (mkEffectFn2 f)

-- || function in lieu of testing null on the array of children
hasChildren :: forall d. HierarchyNode d -> Effect Boolean
hasChildren = runEffectFn1 hasChildrenFn

-- functions to provide access to the parents and children without circular ref in the definition
children :: forall d. HierarchyNode d -> Effect (Array (HierarchyNode d))
children = runEffectFn1 childrenFn

parent  :: forall d. HierarchyNode d -> Effect (Maybe (HierarchyNode d))
parent node = toMaybe <$> runEffectFn1 parentFn node

parentsEq  :: ∀ d. HierarchyNode d -> HierarchyNode d -> Boolean
parentsEq  = parentsEqFn

-- each :: forall d. HierarchyNode d  ->
-- eachAfter :: forall d. HierarchyNode d ->
-- eachBefore :: forall d. HierarchyNode d  ->
-- path :: forall d. HierarchyNode d  ->
-- sort :: forall d. HierarchyNode d  ->
