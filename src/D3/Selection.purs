module D3.Selection
  ( Selection         -- Types
  , CallbackParam
  , CallbackParamP
  , d3Select
  , d3SelectAll
  , append
  , listOfAttr
  , getAttr   -- getters provided separate because they don't return a Selection
  -- , call   -- TBD
  , call, call1, call2, call3, call4, call5, call6, call7, call8, call9
  , dataBindArray
  , dataBindIndexArray
  , dataBindHierarchy
  , dataBindIndexHierarchy
  -- , each   -- TBD
  , empty
  , enter
  , exit
  , insert
  , merge
  , node
  , nodes
  , on
  , on'             -- revisit this and see if can be wrapped in ADT like the other polymorphic functions TODO
  , remove
  , select
  , selectAll
  , selectElem
  , size
  ) where

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, EffFn4, EffFn5, runEffFn1, runEffFn2, runEffFn3, runEffFn5)
import D3.Base (D3, D3Element, Hierarchy, Index)
import DOM.Event.Types (EventType)
import Data.List (List(..), foldM)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe, Nullable)
import Prelude (Unit, bind, pure, (<$>))
import TaglessD3.AttrNew (Attr(..), getTag)

foreign import data Selection :: Type -> Type

-- missing SelectFnFn which takes a predicate fn to perform the selection
foreign import appendFn      :: ∀ d eff.      EffFn2 (d3::D3|eff) String                      (Selection d) (Selection d)
foreign import bindDataFn    :: ∀ d1 d2 eff.  EffFn2 (d3::D3|eff) (Array d2)                  (Selection d1) (Selection d2)
foreign import bindDataFnK   :: ∀ d1 d2 k eff. EffFn3 (d3::D3|eff) (Array d2) (d2 -> k)       (Selection d1) (Selection d2)
foreign import bindHierarchyFn  :: ∀ d1 d2 eff.   EffFn2 (d3::D3|eff) (Hierarchy d2)          (Selection d1) (Selection d2)
foreign import bindHierarchyFnK :: ∀ d1 d2 k eff. EffFn3 (d3::D3|eff) (Hierarchy d2) (d2 -> k) (Selection d1)(Selection d2)
foreign import d3SelectAllFn :: ∀ d eff.      EffFn1 (d3::D3|eff) String                                     (Selection d)
foreign import d3SelectFn    :: ∀ d eff.      EffFn1 (d3::D3|eff) String                                     (Selection d)
foreign import emptyFn       :: ∀ d eff.      EffFn1 (d3::D3|eff)                             (Selection d) Boolean
foreign import enterFn       :: ∀ d eff.      EffFn1 (d3::D3|eff)                             (Selection d) (Selection d)
foreign import exitFn        :: ∀ d eff.      EffFn1 (d3::D3|eff)                             (Selection d) (Selection d)
foreign import filterFn      :: ∀ d eff.      EffFn2 (d3::D3|eff) String                      (Selection d) (Selection d)
foreign import filterFnP     :: ∀ d eff.      EffFn2 (d3::D3|eff) (d -> Boolean)              (Selection d) (Selection d)
foreign import getAttrFn     :: ∀ v d eff.    EffFn2 (d3::D3|eff) String                      (Selection d) v
foreign import insertFn      :: ∀ d eff.      EffFn2 (d3::D3|eff) String                      (Selection d) (Selection d)
foreign import mergeFn       :: ∀ d eff.      EffFn2 (d3::D3|eff) (Selection d)               (Selection d) (Selection d)
foreign import nodeFn        :: ∀ d eff.      EffFn1 (d3::D3|eff)                             (Selection d) (Nullable D3Element)
foreign import nodesFn       :: ∀ d eff.      EffFn1 (d3::D3|eff)                             (Selection d) (Array D3Element)
foreign import orderFn       :: ∀ d eff.      EffFn1 (d3::D3|eff)                             (Selection d) (Selection d)
foreign import removeFn      :: ∀ d eff.      EffFn1 (d3::D3|eff)                             (Selection d) (Selection d)
foreign import selectAllFn   :: ∀ d eff.      EffFn2 (d3::D3|eff) String                      (Selection d) (Selection d)
foreign import selectElFn    :: ∀ d eff.      EffFn1 (d3::D3|eff) D3Element                                 (Selection d) -- is this really in D3? TODO
foreign import selectFn      :: ∀ d eff.      EffFn2 (d3::D3|eff) String                      (Selection d) (Selection d)
foreign import sizeFn        :: ∀ d eff.      EffFn1 (d3::D3|eff)                             (Selection d) Int

type D3CallbackDINE eff d r = (EffFn4 (d3::D3|eff)     -- eff
                                   d                 -- 1st arg of callback
                                   Number            -- 2nd arg of callback
                                   (Array D3Element) -- 3rd arg of callback
                                   D3Element         -- 4th arg of callback
                                   r)                -- result of callback

type D3CallbackText v eff = (EffFn2 (d3::D3|eff)    -- eff
                                      v              -- 1st arg of callback
                                      Index          -- 2nd arg of callback
                                      String)        -- result of callback, a style


-- these foreign functions come in two flavors, simple versions and callback versions
foreign import classedFn  :: ∀ d eff. EffFn3 (d3::D3|eff) String Boolean (Selection d) (Selection d)
foreign import classedFnP :: ∀ d eff. EffFn3 (d3::D3|eff)         -- eff
                                             String               -- 1st arg of classedFnP
                                            (D3CallbackDINE eff d Boolean)  -- 2nd arg is callback
                                            (Selection d)             -- 3rd arg
                                            (Selection d)             -- result

foreign import styleFn   :: ∀ d v eff. EffFn3 (d3::D3|eff) String v (Selection d) (Selection d)
foreign import attrFn  :: ∀ d v eff. EffFn3 (d3::D3|eff) String v (Selection d) (Selection d)

-- | turrrrrns out this is a Nullable value TODO
getAttr :: ∀ v d eff. String                       -> Selection d -> Eff (d3::D3|eff) v
getAttr s                    = runEffFn2 getAttrFn  s

listOfAttr :: ∀ eff d. List Attr -> Selection d -> Eff (d3::D3|eff) (Selection d)
listOfAttr Nil s = pure s
listOfAttr as s = do
    _ <- arrayOfSelections
    pure s
    where
        arrayOfSelections = foldM applyAttr s as
        applyAttr :: Selection d -> Attr -> Eff (d3::D3|eff) (Selection d)
        applyAttr sel at =
            case at of
            (Style stylename _) -> runEffFn3 styleFn stylename at sel
            _ ->  runEffFn3 attrFn (getTag at) at sel

d3Select :: ∀ d eff. String -> Eff (d3::D3|eff) (Selection d)
d3Select selector = runEffFn1 d3SelectFn selector

d3SelectAll :: ∀ d eff. String -> Eff (d3::D3|eff) (Selection d)
d3SelectAll selector = runEffFn1 d3SelectAllFn selector

selectAll :: ∀ d eff. String -> Selection d -> Eff (d3::D3|eff) (Selection d)
selectAll selector = runEffFn2 selectAllFn selector

selectElem :: ∀ d eff. D3Element -> Eff (d3::D3|eff) (Selection d)  -- think this doesn't actually exist...TODO
selectElem element = runEffFn1 selectElFn element

select  :: ∀ d eff.  String -> Selection d -> Eff (d3::D3|eff) (Selection d)
select selector = runEffFn2 selectFn selector

-- would be nice to express that Keyed needs k to be Ord, will have to wait for GADTs
-- dataBind :: ∀ d k eff. Ord k => DataBind d k    -> Selection d -> Eff (d3::D3|eff) (Selection d)
dataBindArray :: forall eff d2 d1. Array d2 -> Selection d1 -> Eff ( d3 :: D3 | eff ) (Selection d2)
dataBindArray d         = runEffFn2 bindDataFn d

dataBindIndexArray :: ∀ d1 d2 k eff. Array d2 -> (d2 -> k) -> Selection d1 -> Eff (d3::D3|eff) (Selection d2)
dataBindIndexArray d keyFn   = runEffFn3 bindDataFnK d keyFn

dataBindHierarchy :: ∀ d1 d2 eff. Hierarchy d2           -> Selection d1 -> Eff (d3::D3|eff) (Selection d2)
dataBindHierarchy h         = runEffFn2 bindHierarchyFn h

dataBindIndexHierarchy :: ∀ d1 d2 k eff. Hierarchy d2 -> (d2 -> k)            -> Selection d1 -> Eff (d3::D3|eff) (Selection d2)
dataBindIndexHierarchy h keyFn  = runEffFn3 bindHierarchyFnK h keyFn

-- filter  :: ∀ d eff.  Filter d                   -> Selection d -> Eff (d3::D3|eff) (Selection d)
-- filter (Selector s)       = runEffFn2 filterFn s
-- filter (Predicate p)      = runEffFn2 filterFnP p

order :: ∀ d eff.                                  Selection d -> Eff (d3::D3|eff) (Selection d)
order                     = runEffFn1 orderFn

enter :: ∀ d eff.                                  Selection d -> Eff (d3::D3|eff) (Selection d)
enter                     = runEffFn1 enterFn

merge :: ∀ d eff.    Selection d                -> Selection d -> Eff (d3::D3|eff) (Selection d)
merge                     = runEffFn2 mergeFn

empty :: ∀ d eff.                                   Selection d -> Eff (d3::D3|eff) Boolean
empty                     = runEffFn1 emptyFn

node :: ∀ d eff.                                   Selection d -> Eff (d3::D3|eff) (Maybe D3Element)
node s                    = toMaybe <$> runEffFn1 nodeFn s

nodes :: ∀ d eff.                                  Selection d -> Eff (d3::D3|eff) (Array D3Element)
nodes                     = runEffFn1 nodesFn

exit :: ∀ d eff.                                   Selection d -> Eff (d3::D3|eff) (Selection d)
exit                      = runEffFn1 exitFn

remove :: ∀ d eff.                                 Selection d -> Eff (d3::D3|eff) (Selection d)  -- maybe this should be Void? TODO
remove                    = runEffFn1 removeFn

size :: ∀ d eff.                                   Selection d -> Eff (d3::D3|eff) Int
size                      = runEffFn1 sizeFn

insert  :: ∀ d eff.  String                     -> Selection d -> Eff (d3::D3|eff) (Selection d)
insert tag                = runEffFn2 insertFn tag

append  :: ∀ d eff.  String                     -> Selection d -> Eff (d3::D3|eff) (Selection d)
append tag                = runEffFn2 appendFn tag

-- using the slightly clunkier syntax of fn(selection, p1, p2) to mirror d3 syntax
call   :: ∀ x eff.
              (Selection x -> Eff (d3::D3|eff)(Selection x))
            -> Selection x -> Eff (d3::D3|eff) (Selection x)
call fn s =  fn s

call1   :: ∀ x a eff.
            (Selection x -> a -> Eff (d3::D3|eff)(Selection x))
            -> a
            -> Selection x -> Eff (d3::D3|eff) (Selection x)
call1 fn a s =  fn s a

call2   :: ∀ x a b eff.
            (Selection x -> a -> b -> Eff (d3::D3|eff)(Selection x))
            -> a -> b
            -> Selection x -> Eff (d3::D3|eff) (Selection x)
call2 fn a b s =  fn s a b

call3   :: ∀ x a b c eff.
            (Selection x -> a -> b -> c -> Eff (d3::D3|eff)(Selection x))
            -> a -> b -> c
            -> Selection x -> Eff (d3::D3|eff) (Selection x)
call3 fn a b c s =  fn s a b c

call4   :: ∀ x a b c d eff.
            (Selection x -> a -> b -> c -> d -> Eff (d3::D3|eff)(Selection x))
            -> a -> b -> c -> d
            -> Selection x -> Eff (d3::D3|eff) (Selection x)
call4 fn a b c d s =  fn s a b c d

call5   :: ∀ x a b c d e eff.
            (Selection x -> a -> b -> c -> d -> e -> Eff (d3::D3|eff)(Selection x))
            -> a -> b -> c -> d -> e
            -> Selection x -> Eff (d3::D3|eff) (Selection x)
call5 fn a b c d e s =  fn s a b c d e

call6   :: ∀ x a b c d e f eff.
            (Selection x -> a -> b -> c -> d -> e -> f -> Eff (d3::D3|eff)(Selection x))
            -> a -> b -> c -> d -> e -> f
            -> Selection x -> Eff (d3::D3|eff) (Selection x)
call6 fn a b c d e f s =  fn s a b c d e f

call7   :: ∀ x a b c d e f g eff.
            (Selection x -> a -> b -> c -> d -> e -> f -> g -> Eff (d3::D3|eff)(Selection x))
            -> a -> b -> c -> d -> e -> f -> g
            -> Selection x -> Eff (d3::D3|eff) (Selection x)
call7 fn a b c d e f g s =  fn s a b c d e f g

call8   :: ∀ x a b c d e f g h eff.
            (Selection x -> a -> b -> c -> d -> e -> f -> g -> h -> Eff (d3::D3|eff)(Selection x))
            -> a -> b -> c -> d -> e -> f -> g -> h
            -> Selection x -> Eff (d3::D3|eff) (Selection x)
call8 fn a b c d e f g h s =  fn s a b c d e f g h

call9   :: ∀ x a b c d e f g h i eff.
            (Selection x -> a -> b -> c -> d -> e -> f -> g -> h -> i -> Eff (d3::D3|eff)(Selection x))
            -> a -> b -> c -> d -> e -> f -> g -> h -> i
            -> Selection x -> Eff (d3::D3|eff) (Selection x)
call9 fn a b c d e f g h i s =  fn s a b c d e f g h i

-- || Callback stuff
-- first up from Graphics.D3.EffFnExtra
type PropertyName = String
type CallbackParam d =
    { datum     :: d
    , elem      :: D3Element
    , timestamp :: Number
    , meta      :: Boolean
    , shift     :: Boolean
    , ctrl      :: Boolean
    , alt       :: Boolean
  }
type CallbackParamP d p =
    { datum     :: d
    , elem      :: D3Element
    , timestamp :: Number
    , prop      :: p
    , meta      :: Boolean
    , shift     :: Boolean
    , ctrl      :: Boolean
    , alt       :: Boolean
  }

foreign import data D3EffCallback      :: # Effect -> Type -> Type -> Type
foreign import data D3EffCallbackP     :: # Effect -> Type -> Type -> Type -> Type
foreign import      mkCallback         :: ∀ eff d r.   (CallbackParam d -> Eff eff r)
  -> D3EffCallback eff (CallbackParam d) r
foreign import      mkCallbackWithProp :: ∀ eff d p r. (CallbackParamP d p -> Eff eff r) -> PropertyName
  -> D3EffCallbackP eff (CallbackParamP d p) PropertyName r

foreign import onFn :: ∀ eff a d.
    EffFn3 (d3::D3|eff)
      (Selection a)               -- 1st argument for EffFn3, the selection itself
      EventType                   -- 2nd argument for EffFn3, the type of the event being bound
      (D3EffCallback (d3::D3|eff) -- 3rd argument for EffFn3, this is the callback function
        (CallbackParam d)           -- arg for callback mkCallback
        Unit)                       -- result of mkCallback
      (Selection a)               -- result of EffFn3, returns selection for "fluid interface" / monadic chain

foreign import onFnWithProperty :: ∀ eff a d p.
  EffFn5 (d3::D3|eff)
        (Selection a)               -- 1st argument for EffFn5, the selection itself
        EventType                   -- 2nd argument for EffFn5, the type of the event being bound
        (D3EffCallbackP (d3::D3|eff)-- 3rd argument for EffFn5, this is the callback function
            (CallbackParamP d p)      -- arg 1 for callback mkCallbackWithProp,
            PropertyName              -- arg 2 for callback mkCallbackWithProp
            Unit)                     --  result of mkCallbackWithProp
        PropertyName                -- 4th argument for EffFn5, name of a property to cache something in
        p                           -- 5th argument for EffFn5, something to cache in the property field
        (Selection a)               -- result of EffFn5, returns selection for "fluid interface" / monadic chain

-- generic "on" function works for any DOM event
on :: ∀ a d eff. EventType
                -> (CallbackParam d -> Eff (d3::D3|eff) Unit)
                -> (Selection a) -> Eff (d3::D3|eff) (Selection a)
on event callback selection  = runEffFn3 onFn selection event (mkCallback callback)

on' :: ∀ a d p eff. EventType -> PropertyName -> p
                -> (CallbackParamP d p -> Eff (d3::D3|eff) Unit)
                -> (Selection a) -> Eff (d3::D3|eff) (Selection a)
on' evType propName prop callback sel = runEffFn5 onFnWithProperty sel evType (mkCallbackWithProp callback propName) propName prop
