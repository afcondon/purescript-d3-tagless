module TaglessD3.AttrNew (
      Attr(..)
    , Attr'
    , D3Attr(..)
    , D3Selection'
    , D3Effect
    , attrValue
    , attrFunction
    , attributes
    , renderArrayOfAttributes
    , getTag
    )where

import Control.Monad.Eff (Eff)
import D3.Base (D3, D3Element)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable)
import Data.Function.Uncurried (mkFn4)
import Data.List (List(..), foldl, fromFoldable, intercalate)
import Prelude (class Show, Unit, const, map, pure, show, unit, ($), (<>))

type D3Effect = ∀ e. Eff (d3 :: D3 | e) Unit

type D3Selection' = Unit -- dummy definition for now
data D3Attr a = D3Attr { value :: a
                       , showValue :: a -> String -- this only produces a String, not enough for actually passing to D3
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
  show (CX a')
    =  "CX: " <> runExists showOne a' -- showOne :: ∀ a. D3Attr a -> String
  show (CY a')
    =  "CY: " <> runExists showOne a'
  show (R a')
    =  "R: " <> runExists showOne a'
  show (X a')
    =  "X: " <> runExists showOne a'
  show (Y a')
    =  "Y: " <> runExists showOne a'
  show (DX a')
    =  "DX: " <> runExists showOne a'
  show (DY a')
    =  "DY: " <> runExists showOne a'
  show (Height a')
    =  "Height: " <> runExists showOne a'
  show (Width a')
    =  "Width: " <> runExists showOne a'
  show (StrokeWidth a')
    =  "StrokeWidth: " <> runExists showOne a'
  show (StrokeOpacity a')
    =  "StrokeOpacity: " <> runExists showOne a'
  show (FillOpacity a')
    =  "FillOpacity: " <> runExists showOne a'
  show (Opacity a')
    =  "Opacity: " <> runExists showOne a'
  show (D a')
    =  "D: " <> runExists showOne a'
  show (Id a')
    =  "Id: " <> runExists showOne a'
  show (StrokeLineCap a')
    =  "StrokeLineCap: " <> runExists showOne a'
  show (PatternUnits a')
    =  "PatternUnits: " <> runExists showOne a'
  show (Style s a')
    =  "Style: " <> s <> " " <> runExists showOne a'
  show (Class a')
    =  "Class: " <> runExists showOne a'
  show (Text a')
    =  "Text: " <> runExists showOne a'
  show (Type a')
    =  "Type: " <> runExists showOne a'

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

-- constructors for attribute values, both values and callback functions
attrValue :: ∀ a. Show a => a -> Attr'
attrValue v = mkExists (D3Attr { value: v, showValue: show })

attrFunction :: ∀ d r. (d -> Number -> Array D3Element -> D3Element -> r) -> Attr'
attrFunction f = mkExists (D3Attr { value: mkFn4 f, showValue: const "(function)" })

showAll :: List Attr' -> List String
showAll = map (runExists showOne)

showOne :: ∀ a. D3Attr a -> String
showOne (D3Attr ops) = ops.showValue ops.value


getTag :: Attr -> String
getTag a =
    case a of
    (CX _)            -> "cx"
    (CY _)            -> "CY"
    (R _)             -> "R"
    (X _)             -> "X"
    (Y _)             -> "Y"
    (DX _)            -> "DX"
    (DY _)            -> "DY"
    (Height _)        -> "Height"
    (Width _)         -> "Width"
    (StrokeWidth _)   -> "StrokeWidth"
    (StrokeOpacity _) -> "StrokeOpacity"
    (FillOpacity _)   -> "FillOpacity"
    (Opacity _)       -> "Opacity"
    (D _)             -> "D"
    (Id _)            -> "Id"
    (StrokeLineCap _) -> "StrokeLineCap"
    (PatternUnits _)  -> "PatternUnits"
    (Text _)          -> "Text"
    (Type _)          -> "Type"
    (Style _ _)       -> "Style" -- refactor so that different FFI is called TODO
    (Class _)         -> "Class" -- refactor so that different FFI is called TODO
