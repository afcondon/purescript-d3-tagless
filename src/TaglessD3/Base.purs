module TaglessD3.Base where

import Data.Newtype (class Newtype)
import Prelude (class Show, show, (<>))
import Control.Apply (applySecond)
import D3.Base

-- can be used to provide closer match to the JavaScript D3 syntax if desired
infixl 4 applySecond as ..  -- maybe clearer than *>

type Selector = String

data Duration = Seconds Int | MS Int

data Callback d b =   Lambda1 (d ->                                  b)
                    | Lambda2 (d -> Number ->                        b)
                    | Lambda4 (d -> Number -> Peers -> DomElement -> b)

data D3Transition = SimpleTransition Duration
                  | NamedTransition String Duration

newtype Hierarchy d = Hierarchy { name :: String, children :: Array (Hierarchy d), datum :: d }

derive instance newtypeHierarchy :: Newtype (Hierarchy d) _

data D3ElementType
    =     SvgCircle
        -- | SvgEllipse
        | SvgGroup
        | SvgImage
        -- | SvgLine
        -- | SvgMesh
        | SvgPath
        -- | SvgPolygon
        -- | SvgPolyline
        | SvgRect
        | SvgText
        -- | SvgUse

-- static evaluation of SVGPathString could be done in dummy interpreter or,
-- more ambitiously it could be built up using further DSL mechanics
type SVGPathString = String


instance showDuration :: Show Duration where
    show (Seconds i) = show i  <> "s"
    show (MS ms)     = show ms <> "ms"

instance showD3Transition :: Show D3Transition where
  show (SimpleTransition t)     = "Duration: " <> show t
  show (NamedTransition name t) = "Name: " <> name <> " Duration: " <> show t

instance showD3ElementType :: Show D3ElementType where
  show SvgCircle = "circle"
  show SvgGroup  = "group"
  show SvgImage  = "image"
  show SvgPath   = "path"
  show SvgRect   = "rect"
  show SvgText   = "text"
