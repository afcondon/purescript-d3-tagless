module TaglessD3.AttrNew (
      Attr(..)
    , Attr'
    , D3Attr
    , attrInt
    , attrIntP
    , attrChar
    , attrCharP
    , attrString
    , attrStringP
    , renderArrayOfAttributes
    )where

import Control.Monad.Eff (Eff)
import D3.Base (D3)
import Data.Exists (Exists, mkExists, runExists)
import Data.List (List, foldl, fromFoldable)
import Data.Traversable (traverse)
import Prelude (class Show, Unit, bind, const, map, pure, show, unit, ($), (<>))

type D3Effect = ∀ e. Eff (d3 :: D3 | e) Unit

type D3Selection' = Unit -- dummy definition for now
data D3Attr a = D3Attr { value :: a
                       , showValue :: a -> String
                       , apply :: D3Selection' -> a -> D3Effect
                    }
type Attr' = Exists D3Attr

dummyD3Op :: ∀ a. D3Selection' -> a -> D3Effect
dummyD3Op s a = pure unit

dummyD3OpF :: ∀ d a. D3Selection' -> (d -> a) -> D3Effect
dummyD3OpF s fa = pure unit

renderArrayOfAttributes :: List Attr -> String
renderArrayOfAttributes attrs = foldl go "" attrs
    where
    go :: String -> Attr -> String
    go acc attr = show attr <> acc

instance showD3Attr :: Show (D3Attr a) where
  show (D3Attr { value, showValue }) = "Attr a: " <> showValue value

instance showAttr :: Show Attr where
  show (CX a) =  "CX" <> runExists showOne a
  show (CY _) =  "CY"
  show (R _)  =  "R"
  show (X _)  =  "X"
  show (Y _)  =  "Y"
  show (DX _) =  "DX"
  show (DY _) =  "DY"
  show (Height _) =  "Height"
  show (Width _)  =  "Width"
  show (StrokeWidth _)   =  "StrokeWidth"
  show (StrokeOpacity _) =  "StrokeOpacity"
  show (FillOpacity _)   =  "FillOpacity"
  show (Opacity _)       =  "Opacity"
  show (D _)  =  "D"
  show (Id _) =  "Id"
  show (StrokeLineCap _) =  "StrokeLineCap"
  show (PatternUnits _)  =  "PatternUnits"
  show (Style _ _) =  "Style"
  show (Class _)   =  "Class"
  show (Text _)    =  "Text"
  show (Type _)    =  "Type"

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
attrInt :: Int -> Attr'
attrInt i = mkExists (D3Attr { value: i, showValue: show, apply: dummyD3Op })

attrChar :: Char -> Attr'
attrChar c = mkExists (D3Attr { value: c, showValue: show, apply: dummyD3Op })

attrString :: String -> Attr'
attrString s = mkExists (D3Attr { value: s, showValue: show, apply: dummyD3Op })

-- now constructors for Attributes that are indirect - take a callback from another type
attrIntP :: ∀ d. (d -> Int) -> Attr'
attrIntP fi = mkExists (D3Attr { value: fi, showValue: const "(function)", apply: dummyD3Op })

attrCharP :: ∀ d. (d -> Char) -> Attr'
attrCharP fc = mkExists (D3Attr { value: fc, showValue: const "(function)", apply: dummyD3Op })

attrStringP :: ∀ d. (d -> String) -> Attr'
attrStringP fs = mkExists (D3Attr { value: fs, showValue: const "(function)", apply: dummyD3Op })

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
showAll :: List Attr' -> List String
showAll = map (runExists showOne)

showOne :: ∀ a. D3Attr a -> String
showOne (D3Attr ops) = ops.showValue ops.value
