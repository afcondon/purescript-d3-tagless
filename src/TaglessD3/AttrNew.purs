module TaglessD3.AttrNew where

import Control.Monad.Eff (Eff)
import D3.Base (D3)
import Data.Exists (Exists, mkExists, runExists)
import Data.List (List, fromFoldable)
import Data.Traversable (traverse)
import Prelude (Unit, bind, pure, unit, ($))

type D3Effect = ∀ e. Eff (d3 :: D3 | e) Unit

type D3Selection' = Unit -- dummy definition for now

data D3Attr a = D3Attr { value :: a
                       , apply :: D3Selection' -> a -> D3Effect
                    }

dummyD3Op :: ∀ a. D3Selection' -> a -> D3Effect
dummyD3Op s a = pure unit

dummyD3OpF :: ∀ d a. D3Selection' -> (d -> a) -> D3Effect
dummyD3OpF s fa = pure unit

-- constructors to make Attributes for primitive types, used as direct value
mkAttrInt :: Int -> Exists D3Attr
mkAttrInt i = mkExists (D3Attr { value: i, apply: dummyD3Op })

mkAttrChar :: Char -> Exists D3Attr
mkAttrChar c = mkExists (D3Attr { value: c, apply: dummyD3Op })

mkAttrString :: String -> Exists D3Attr
mkAttrString s = mkExists (D3Attr { value: s, apply: dummyD3Op })

-- now constructors for Attributes that are indirect - take a callback from another type
mkAttrIntP :: ∀ d. (d -> Int) -> Exists D3Attr
mkAttrIntP fi = mkExists (D3Attr { value: fi, apply: dummyD3Op })

mkAttrCharP :: ∀ d. (d -> Char) -> Exists D3Attr
mkAttrCharP fc = mkExists (D3Attr { value: fc, apply: dummyD3Op })

mkAttrStringP :: ∀ d. (d -> String) -> Exists D3Attr
mkAttrStringP fs = mkExists (D3Attr { value: fs, apply: dummyD3Op })


-- an example list of attributes and an example main operating on them
attrList :: List (Exists D3Attr)
attrList = fromFoldable $ [ mkAttrInt 1, mkAttrString "foo", mkAttrCharP lp ]

-- an example of a typed callback function
type ExData = { name :: String, age :: Int }
lp :: ExData -> Char
lp { name, age } =
    case name, age of
    "awn", _ -> 'a'
    _, 0     -> 'b'
    _, _     -> 'c'

main2 :: D3Effect
main2 = do
    applyAll unit attrList

applyAll :: D3Selection' -> List (Exists D3Attr) -> D3Effect
applyAll s list = do
    traverse (runExists applyOne) list
    pure unit
  where
    applyOne :: ∀ a. (D3Attr a) -> D3Effect
    applyOne (D3Attr ops) = ops.apply s ops.value


-- updateAll :: List (Exists D3Attr) -> List (Exists D3Attr)
-- updateAll =
--     map (runExists updateOne)
--   where
--     updateOne :: ∀ a. D3Attr a -> Exists D3Attr
--     updateOne (D3Attr ops) = mkExists (D3Attr ( { apply: ops.apply
--                                         , update: ops.update
--                                         , value: ops.update ops.value } ))
