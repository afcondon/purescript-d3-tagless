module TaglessD3.AttrNew (
      Attr(..)
    , Attr'
    , D3Attr(..)
    , D3Selection'
    , D3Effect
    , attrInt
    , attrIntP
    , attrChar
    , attrCharP
    , attrString
    , attrStringP
    , attributes
    , renderArrayOfAttributes
    )where

import Control.Monad.Eff (Eff)
import D3.Base (D3)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable)
import Data.List (List(..), foldl, fromFoldable, intercalate)
import Prelude (class Show, Unit, const, map, pure, show, unit, ($), (<>))

type D3Effect = ∀ e. Eff (d3 :: D3 | e) Unit

type D3Selection' = Unit -- dummy definition for now
data D3Attr a = D3Attr { value :: a
                       , showValue :: a -> String -- this only produces a String, not enough for actually passing to D3
                       , apply :: D3Selection' -> a -> D3Effect
                    }
type Attr' = Exists D3Attr

dummyD3Op :: ∀ a. D3Selection' -> a -> D3Effect
dummyD3Op s a = pure unit

dummyD3OpF :: ∀ d a. D3Selection' -> (d -> a) -> D3Effect
dummyD3OpF s fa = pure unit

renderArrayOfAttributes :: List Attr -> String
renderArrayOfAttributes attrs = intercalate ", " $ foldl go Nil attrs
    where
    go :: List String -> Attr -> List String
    go acc attr = Cons (show attr) acc

instance showD3Attr :: Show (D3Attr a) where
  show (D3Attr { value, showValue }) = "Attr a: " <> showValue value

instance showAttr :: Show Attr where
  show (CX a)
    =  "CX: " <> runExists showOne a
  show (CY a)
    =  "CY: " <> runExists showOne a
  show (R a)
    =  "R: " <> runExists showOne a
  show (X a)
    =  "X: " <> runExists showOne a
  show (Y a)
    =  "Y: " <> runExists showOne a
  show (DX a)
    =  "DX: " <> runExists showOne a
  show (DY a)
    =  "DY: " <> runExists showOne a
  show (Height a)
    =  "Height: " <> runExists showOne a
  show (Width a)
    =  "Width: " <> runExists showOne a
  show (StrokeWidth a)
    =  "StrokeWidth: " <> runExists showOne a
  show (StrokeOpacity a)
    =  "StrokeOpacity: " <> runExists showOne a
  show (FillOpacity a)
    =  "FillOpacity: " <> runExists showOne a
  show (Opacity a)
    =  "Opacity: " <> runExists showOne a
  show (D a)
    =  "D: " <> runExists showOne a
  show (Id a)
    =  "Id: " <> runExists showOne a
  show (StrokeLineCap a)
    =  "StrokeLineCap: " <> runExists showOne a
  show (PatternUnits a)
    =  "PatternUnits: " <> runExists showOne a
  show (Style s a)
    =  "Style: " <> s <> " " <> runExists showOne a
  show (Class a)
    =  "Class: " <> runExists showOne a
  show (Text a)
    =  "Text: " <> runExists showOne a
  show (Type a)
    =  "Type: " <> runExists showOne a

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


attributes :: ∀ f a. (Foldable f) => f a -> List a
attributes = fromFoldable


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

showAll :: List Attr' -> List String
showAll = map (runExists showOne)

showOne :: ∀ a. D3Attr a -> String
showOne (D3Attr ops) = ops.showValue ops.value
