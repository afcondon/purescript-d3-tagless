module D3.ForceSimulation where

-- import Data.Pair

import D3.Base (Index)
import D3.Selection (Selection)
import Data.Function.Uncurried (mkFn2, Fn2)
import Data.Maybe (Maybe(Nothing, Just))
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn1, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Prelude (Unit)


foreign import data D3Simulation :: Type
foreign import data D3Force      :: Type

type Node = { id :: String, group :: Number }
type Link = { source :: String, target :: String, value :: Number }

type GroupedForceLayout = { nodes :: Array Node
                          , links :: Array Link }

-- for a type to be draggable it will have to have x and y fields for the drag callback to operate on
type ForceNode = { id :: String, group :: Number, x :: Number, y :: Number }
type ForceLink = { source :: ForceNode, target :: ForceNode, value :: Number }
type DraggableLayout = { nodes :: Array ForceNode
                       , links :: Array Link }

data ForceType      = Centering | Collision | Links | ManyBody | ForceX | ForceY
data SimulationType = Force

foreign import addForceFn          :: EffectFn3 String D3Force D3Simulation    D3Simulation
foreign import d3ForceSimulationFn :: Effect                                   D3Simulation
foreign import linkIDFn            :: ∀ v. EffectFn2 (Fn2 Node Index v) D3Force      D3Force
foreign import makeCenterForceFn   :: Effect                                        D3Force
foreign import makeCenterForceFnP  :: EffectFn1 (Array Number)                      D3Force
foreign import makeLinkForceFn     :: EffectFn1 (Array Link)                        D3Force
foreign import makeManyBodyForceFn :: Effect                                        D3Force
foreign import getLinksFn          :: EffectFn1 D3Simulation                   (Array Link)
foreign import setLinksFn          :: EffectFn2 (Array Link) D3Force                D3Force
foreign import getForceFn          :: EffectFn2 String       D3Simulation           D3Force
foreign import simulationNodesFn   :: EffectFn2 (Array Node) D3Simulation      D3Simulation
foreign import onTickFn            :: EffectFn2
                                      (Effect Unit)
                                      D3Simulation
                                      D3Simulation
foreign import defaultTickFn       :: EffectFn2 (Selection Node) (Selection Link)      Unit

defaultTick :: Selection Node -> Selection Link -> Effect Unit
defaultTick = runEffectFn2 defaultTickFn

d3ForceSimulation :: SimulationType -> Effect D3Simulation
d3ForceSimulation Force = d3ForceSimulationFn

initNodes :: Array Node -> D3Simulation -> Effect D3Simulation
initNodes = runEffectFn2 simulationNodesFn

onTick  :: Effect Unit -> D3Simulation -> Effect D3Simulation
onTick = runEffectFn2 onTickFn

addForce :: ForceType -> String -> D3Force -> D3Simulation -> Effect D3Simulation
addForce Centering = runEffectFn3 addForceFn
addForce Collision = runEffectFn3 addForceFn -- "not implemented yet"
addForce Links     = runEffectFn3 addForceFn
addForce ManyBody  = runEffectFn3 addForceFn
addForce ForceX    = runEffectFn3 addForceFn -- "not implemented yet"
addForce ForceY    = runEffectFn3 addForceFn -- "not implemented yet"

-- This function will blow up if String doens't look up a valid force on this simulation TODO
getForce :: String -> D3Simulation                         -> Effect D3Force
getForce name sim = runEffectFn2 getForceFn name sim

-- || functions only for LINK force
makeLinkForce :: Maybe (Array Link) -> Effect D3Force
makeLinkForce (Just ls) = runEffectFn1 makeLinkForceFn   ls
makeLinkForce Nothing   = runEffectFn1 makeLinkForceFn   []

setIDFunction :: ∀ v. (Node -> Index -> v) -> D3Force -> Effect D3Force -- Force HAS TO BE LINK FORCE HERE
setIDFunction f = runEffectFn2 linkIDFn (mkFn2 f)

setLinks      :: (Array Link) -> D3Force -> Effect D3Force
setLinks      = runEffectFn2 setLinksFn

getLinks      :: D3Simulation            -> Effect (Array Link)
getLinks      = runEffectFn1 getLinksFn

-- || functions only for MANY BODY force
makeManyBody :: Effect D3Force
makeManyBody = makeManyBodyForceFn

-- || functions only for CENTERING force was using Data.Pair to be a little more
-- precise but dropped this to get it to compile with psc-package which doesn't
-- yet have that
makeCenterForce :: Maybe (Array Number) -> Effect D3Force
makeCenterForce (Just xy) = runEffectFn1 makeCenterForceFnP xy
makeCenterForce Nothing   = makeCenterForceFn

{-
Simulation API
  on
  find
  stop
  tick
  alpha
  force
  nodes
  restart
  alphaMin
  alphaDecay
  alphaTarget
  velocityDecay

Simulations typically compose multiple forces as desired. This module provides several for your enjoyment:

                x y radius strength iterations links id distance theta distanceMin distanceMax
  Centering     * *
  Collision           *       *         *
  Links                       *         *        *     *     *
  Many-Body                   *                                     *       *          *
  PositioningX  *             *
  PositioningY    *           *

  Constructors for forces
    Centering   [x,y]
    Collision   [radius]
    Links       [Links]
    ManyBody
    ForceX      [x]
    ForceY      [y]
-}
