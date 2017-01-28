module TaglessD3.DOMImpl where

import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, class Show, Unit, ap, show, unit, ($), (<<<), (<>))
import TaglessD3.Base (Attr, D3ElementType, D3Transition, Hierarchy, Selector, renderArrayOfAttributes)
import TaglessD3.Selection (class Selection)

data D3DOMStructure = D3S String

instance showD3DOMStructure :: Show D3DOMStructure where
  show (D3S name) = "Selection: " <> name

data D3Selection a = D3Selection (D3DOMStructure -> Tuple a D3DOMStructure)

run :: ∀ a. D3Selection a -> D3DOMStructure -> a
run (D3Selection f) = fst <<< f

run' :: ∀ a. D3Selection a -> D3DOMStructure -> D3DOMStructure
run' (D3Selection f) = snd <<< f

instance functorD3Selection :: Functor D3Selection where
  map f (D3Selection g) = D3Selection $ first f <<< g

instance applyD3Selection :: Apply D3Selection where
  apply = ap

instance applicativeD3Selection :: Applicative D3Selection where
  pure a = D3Selection \z -> Tuple a z

instance bindD3Selection :: Bind D3Selection where
  bind (D3Selection f) k = D3Selection $ \z0 ->
    let az1 = f z0
    in (d3StructureFn $ k $ fst az1) (snd az1)

d3StructureFn :: ∀ a. D3Selection a -> (D3DOMStructure -> Tuple a D3DOMStructure)
d3StructureFn (D3Selection f) = f

instance monadD3Selection :: Monad D3Selection

instance selectionDummySelection :: Selection D3Selection where
    d3Select selector    = D3Selection $ d3Select' selector
    d3SelectAll selector = D3Selection $ d3SelectAll' selector
    select selector      = D3Selection $ select' selector
    selectAll selector   = D3Selection $ selectAll' selector
    merge selection      = D3Selection $ merge' selection
    insert element       = D3Selection $ insert' element
    append element       = D3Selection $ append' element
    remove               = D3Selection $ remove'
    enter                = D3Selection $ enter'
    exit                 = D3Selection $ exit'
    attrs attributes     = D3Selection $ attrs' attributes
    transition t         = D3Selection $ transition' t
    dataA ds             = D3Selection $ dataA' ds
    dataH hd             = D3Selection $ dataH' hd
    dataAI ds index      = D3Selection $ dataAI' ds index
    dataHI hd index      = D3Selection $ dataHI' hd index

d3Select' :: Selector -> D3DOMStructure -> Tuple Unit D3DOMStructure
d3Select' selector d3s = Tuple unit $ d3s ++ ["D3Select", selector]

d3SelectAll' :: Selector -> D3DOMStructure -> Tuple Unit D3DOMStructure
d3SelectAll' selector d3s = Tuple unit $ d3s ++ ["D3SelectAll", selector]

select' :: Selector -> D3DOMStructure -> Tuple Unit D3DOMStructure
select' selector d3s = Tuple unit $ d3s ++ ["select", selector]

selectAll' :: Selector -> D3DOMStructure -> Tuple Unit D3DOMStructure
selectAll' selector d3s = Tuple unit $ d3s ++ ["selectAll", selector]

insert' :: D3ElementType -> D3DOMStructure -> Tuple Unit D3DOMStructure
insert' element d3s = Tuple unit $ d3s ++ [ "insert", show element ]

append' :: D3ElementType -> D3DOMStructure -> Tuple Unit D3DOMStructure
append' element d3s = Tuple unit $ d3s ++ [ "append", show element ]

remove' :: D3DOMStructure -> Tuple Unit D3DOMStructure
remove' d3s = Tuple unit $ d3s ++ ["Remove"]

enter' :: D3DOMStructure -> Tuple Unit D3DOMStructure
enter' d3s = Tuple unit $ d3s ++ ["Enter"]

exit' :: D3DOMStructure -> Tuple Unit D3DOMStructure
exit' d3s = Tuple unit $ d3s ++ ["Exit"]

attrs' :: ∀ d. Array (Attr d) -> D3DOMStructure -> Tuple Unit D3DOMStructure
attrs' as d3s = Tuple unit $ d3s ++ [ "Attributes: ", renderArrayOfAttributes as ]

transition' :: D3Transition -> D3DOMStructure -> Tuple Unit D3DOMStructure
transition' t d3s = Tuple unit $ d3s ++ [ show t ]

dataA' :: ∀ d. Array d -> D3DOMStructure -> Tuple (Array d) D3DOMStructure
dataA' ds d3s = Tuple ds $ d3s ++ ["Data from Array"]

dataH' :: ∀ d. Hierarchy d -> D3DOMStructure -> Tuple (Hierarchy d) D3DOMStructure
dataH' hd d3s = Tuple hd $ d3s ++ ["Hierarchical data"]

dataAI' :: ∀ d i. Array d -> (d -> i) -> D3DOMStructure -> Tuple (Array d) D3DOMStructure
dataAI' ds index d3s = Tuple ds $ d3s ++ ["Data from Array with index function"]

dataHI' :: ∀ d i. Hierarchy d -> (d -> i) -> D3DOMStructure -> Tuple (Hierarchy d) D3DOMStructure
dataHI' hd index d3s = Tuple hd $ d3s ++ ["Hierarchical data with index function"]

merge' :: D3Selection Unit -> D3DOMStructure -> (Tuple Unit D3DOMStructure)
merge' (D3Selection f) (D3S name) = Tuple unit (D3S name)

-- | Utility functions
addD3Statement :: D3DOMStructure -> Array String -> D3DOMStructure
addD3Statement (D3S name) statements' = D3S name

infixl 4 addD3Statement as ++
