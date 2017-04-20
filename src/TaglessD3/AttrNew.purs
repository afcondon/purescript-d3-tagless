module TaglessD3.AttrNew (
      Attr(..)
    , Attr'
    , AttrSetter
    , D3Attr(..)
    , D3Selection'
    , D3Effect
    , attrValue
    , attrFunction
    , attributes
    , renderArrayOfAttributes
    , getTag
    , CssUnit(..)
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
                       , units :: String
                    }
type Attr' = Exists D3Attr

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
          | Text                Attr'
          | Type                Attr' -- images only
          | Style        String Attr'
          | Class               Attr'
          | Property     String Attr'
        --   | Fill                (ValueOrCallback d Color)
        --   | Stroke              (ValueOrCallback d Color)

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
  show (Text a')
    =  "Text: " <> runExists showOne a'
  show (Type a')
    =  "Type: " <> runExists showOne a'
  show (Style s a')
    =  "Style: " <> s <> " " <> runExists showOne a'
  show (Class a')
    =  "Class: " <> runExists showOne a'
  show (Property s a')
    =  "Property: " <> s <> " " <> runExists showOne a'

attributes :: ∀ f a. (Foldable f) => f a -> List a
attributes = fromFoldable

-- constructors for attribute values, both values and callback functions
type AttrSetter d v = d -> Number -> (Array D3Element) -> D3Element -> v

attrValue :: ∀ a. Show a => a -> Attr'
attrValue v = mkExists (D3Attr { value: v, showValue: show, units: "" })

data CssUnit = Px | Em | Pc | Rem | None
instance showCssUnit :: Show CssUnit where
  show Px = "px"
  show Em = "em"
  show Rem = "rem"
  show Pc = "%"
  show None = ""

attrFunction :: ∀ d r. CssUnit -> AttrSetter d r -> Attr'
attrFunction u f = mkExists (D3Attr { value: mkFn4 f, showValue: const "(function)", units: show u })

showAll :: List Attr' -> List String
showAll = map (runExists showOne)

showOne :: ∀ a. D3Attr a -> String
showOne (D3Attr ops) = ops.showValue ops.value


getTag :: Attr -> String
getTag a =
    case a of
    (CX _)            -> "cx"
    (CY _)            -> "cy"
    (R _)             -> "r"
    (X _)             -> "x"
    (Y _)             -> "y"
    (DX _)            -> "dx"
    (DY _)            -> "dy"
    (Height _)        -> "height"
    (Width _)         -> "width"
    (StrokeWidth _)   -> "strokewidth"
    (StrokeOpacity _) -> "strokeopacity"
    (FillOpacity _)   -> "fillopacity"
    (Opacity _)       -> "opacity"
    (D _)             -> "d"
    (Id _)            -> "id"
    (StrokeLineCap _) -> "strokelinecap"
    (PatternUnits _)  -> "patternunits"
    (Text _)          -> "text"
    (Type _)          -> "type"
    (Style _ _)       -> "style" -- refactor so that different FFI is called TODO
    (Class _)         -> "class" -- refactor so that different FFI is called TODO
    (Property _ _)    -> "property" -- refactor so that different FFI is called TODO
