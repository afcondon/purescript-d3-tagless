module TaglessD3.Transition where

import Data.List (List)
import Prelude (class Monad, Unit)
import TaglessD3.AttrNew (Attr)
import TaglessD3.Base (D3ElementType, D3Transition, Selector)
import TaglessD3.Selection

class (Monad m) <= AbstractTransition m where
    attrs       :: List Attr     -> (m Unit)
    merge       :: m Unit        -> (m Unit)
    remove      ::                  (m Unit)
    select      :: Selector      -> (m Unit)
    selectAll   :: Selector      -> (m Unit)
    transition  :: D3Transition  -> (m Unit)
-- the following methods on D3's transition are not currently implemented in the tagless version
    -- attrTween
    -- call
    -- d3Interrupt
    -- delay
    -- duration
    -- each
    -- ease
    -- empty
    -- filter
    -- node(s)
    -- on
    -- selection
    -- size
    -- style(s) -- part of attr(s)
    -- styleTween
    -- text
    -- transition
    -- tween
