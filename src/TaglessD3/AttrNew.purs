module D3Tagless.AttrNew where

import Control.Monad.Eff (Eff)
import D3.Base (D3, D3Selection)
import Data.Exists (Exists, mkExists, runExists)
import Data.List (List, fromFoldable)
import Data.Traversable (traverse)
import Prelude (Unit, bind, map, pure, unit, ($), (+), (<>))

type D3Effect = ∀ e. Eff (d3 :: D3 | e) Unit

main2 :: D3Effect
main2 = do
    applyAll unit attrList

type D3Selection' = Unit -- dummy definition for now

dummyD3Op :: ∀ a. D3Selection' -> a -> D3Effect
dummyD3Op s a = pure unit

data D3Attr a = D3Attr { "data" :: a
                       , apply :: D3Selection' -> a -> D3Effect
                    }

attrList :: List (Exists D3Attr)
attrList = fromFoldable $ [ mkExists (D3Attr { "data": 1
                                              , apply: dummyD3Op
                                              })
                           , mkExists (D3Attr { "data": "foo"
                                              , apply: dummyD3Op
                                              })
                           ]

applyAll :: D3Selection' -> List (Exists D3Attr) -> D3Effect
applyAll s list = do
    traverse (runExists applyOne) list
    pure unit
  where
    applyOne :: ∀ a. (D3Attr a) -> D3Effect
    applyOne (D3Attr ops) = ops.apply s ops."data"


-- updateAll :: List (Exists D3Attr) -> List (Exists D3Attr)
-- updateAll =
--     map (runExists updateOne)
--   where
--     updateOne :: ∀ a. D3Attr a -> Exists D3Attr
--     updateOne (D3Attr ops) = mkExists (D3Attr ( { apply: ops.apply
--                                         , update: ops.update
--                                         , "data": ops.update ops."data" } ))
