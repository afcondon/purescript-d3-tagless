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
type Attr' = Exists D3Attr

dummyD3Op :: ∀ a. D3Selection' -> a -> D3Effect
dummyD3Op s a = pure unit

dummyD3OpF :: ∀ d a. D3Selection' -> (d -> a) -> D3Effect
dummyD3OpF s fa = pure unit

renderArrayOfAttributes :: List Attr' -> String
renderArrayOfAttributes attrs = "pending"

data Attr = CX                  Attr' -- circles only
          | CY                  Attr' -- circles only
          | R                   Attr' -- circles only
          | X                   Attr'
          | Y                   Attr'
          | DX                  Attr' -- string because it might have units with
          | DY                  Attr'
          | Height              Attr'
          | Width               Attr'
          | StrokeWidth         Attr'
          | StrokeOpacity       Attr'
          | FillOpacity         Attr'
          | Opacity             Attr'
          | D                   Attr' -- has to be an SVGPathString
          | Id                  Attr'
          | StrokeLineCap       Attr' -- Round | Butt | Square
          | PatternUnits        Attr' -- userSpaceOnUse | objectBoundingBox
          | Style        String Attr'
          | Class               Attr'
          | Text                Attr'
          | Type                Attr' -- images only
        --   | Fill                (ValueOrCallback d Color)
        --   | Stroke              (ValueOrCallback d Color)


-- constructors to make Attributes for primitive types, used as direct value
mkAttrInt :: Int -> Attr'
mkAttrInt i = mkExists (D3Attr { value: i, apply: dummyD3Op })

mkAttrChar :: Char -> Attr'
mkAttrChar c = mkExists (D3Attr { value: c, apply: dummyD3Op })

mkAttrString :: String -> Attr'
mkAttrString s = mkExists (D3Attr { value: s, apply: dummyD3Op })

-- now constructors for Attributes that are indirect - take a callback from another type
mkAttrIntP :: ∀ d. (d -> Int) -> Attr'
mkAttrIntP fi = mkExists (D3Attr { value: fi, apply: dummyD3Op })

mkAttrCharP :: ∀ d. (d -> Char) -> Attr'
mkAttrCharP fc = mkExists (D3Attr { value: fc, apply: dummyD3Op })

mkAttrStringP :: ∀ d. (d -> String) -> Attr'
mkAttrStringP fs = mkExists (D3Attr { value: fs, apply: dummyD3Op })


-- an example list of attributes and an example main operating on them
attrList :: List Attr
attrList = fromFoldable $ [ CX $ mkAttrInt 1, Style "width" $ mkAttrString "48%" ]

-- an example of a typed callback function
type ExData = { name :: String, age :: Int }
lp :: ExData -> Char
lp { name, age } =
    case name, age of
    "awn", _ -> 'a'
    _, 0     -> 'b'
    _, _     -> 'c'

-- main2 :: D3Effect
-- main2 = do
--     applyAll unit ?attrList
--
-- applyAll :: D3Selection' -> List Attr -> D3Effect
-- applyAll s list = do
--     traverse (runExists applyOne) list
--     pure unit
--   where
--     applyOne :: ∀ a. (D3Attr a) -> D3Effect
--     applyOne (D3Attr ops) = ops.apply s ops.value
--
--
-- updateAll :: List Attr' -> List Attr'
-- updateAll =
--     map (runExists updateOne)
--   where
--     updateOne :: ∀ a. D3Attr a -> Exists D3Attr
--     updateOne (D3Attr ops) = mkExists (D3Attr ( { apply: ops.apply
--                                         , update: ops.update
--                                         , value: ops.update ops.value } ))
