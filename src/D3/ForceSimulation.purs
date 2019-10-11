module D3.ForceSimulation where

-- import Data.Pair
import D3.Base (Index, D3, Eff)
import D3.Selection (Selection)
import Effect.Uncurried (EffectFn2, EffectFn1, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Data.Function.Uncurried (mkFn2, Fn2)
import Data.Maybe (Maybe(Nothing, Just))
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

foreign import addForceFn          :: ∀ eff. EffectFn3 String D3Force D3Simulation    D3Simulation
foreign import d3ForceSimulationFn :: ∀ eff. Eff    (d3::D3|eff)                                D3Simulation
foreign import linkIDFn            :: ∀ v eff. EffectFn2 (Fn2 Node Index v) D3Force      D3Force
foreign import makeCenterForceFn   :: ∀ eff. Eff    (d3::D3|eff)                                     D3Force
foreign import makeCenterForceFnP  :: ∀ eff. EffectFn1 (Array Number)                      D3Force
foreign import makeLinkForceFn     :: ∀ eff. EffectFn1 (Array Link)                        D3Force
foreign import makeManyBodyForceFn :: ∀ eff. Eff    (d3::D3|eff)                                     D3Force
foreign import getLinksFn          :: ∀ eff. EffectFn1 D3Simulation                   (Array Link)
foreign import setLinksFn          :: ∀ eff. EffectFn2 (Array Link) D3Force                D3Force
foreign import getForceFn          :: ∀ eff. EffectFn2 String       D3Simulation           D3Force
foreign import simulationNodesFn   :: ∀ eff. EffectFn2 (Array Node) D3Simulation      D3Simulation
foreign import onTickFn            :: ∀ eff. EffectFn2
                                                    (Effect Unit)
                                                    D3Simulation
                                                    D3Simulation
foreign import defaultTickFn       :: ∀ eff. EffectFn2 (Selection Node) (Selection Link)      Unit

defaultTick :: ∀ eff. Selection Node -> Selection Link -> Effect Unit
defaultTick = runEffectFn2 defaultTickFn

d3ForceSimulation :: ∀ eff. SimulationType -> Effect D3Simulation
d3ForceSimulation Force = d3ForceSimulationFn

initNodes :: ∀ eff. Array Node -> D3Simulation -> Effect D3Simulation
initNodes = runEffectFn2 simulationNodesFn

onTick  :: forall eff. Effect Unit -> D3Simulation -> Effect D3Simulation
onTick = runEffectFn2 onTickFn

addForce :: ∀ eff. ForceType -> String -> D3Force -> D3Simulation -> Effect D3Simulation
addForce Centering = runEffectFn3 addForceFn
addForce Collision = runEffectFn3 addForceFn -- "not implemented yet"
addForce Links     = runEffectFn3 addForceFn
addForce ManyBody  = runEffectFn3 addForceFn
addForce ForceX    = runEffectFn3 addForceFn -- "not implemented yet"
addForce ForceY    = runEffectFn3 addForceFn -- "not implemented yet"

-- This function will blow up if String doens't look up a valid force on this simulation TODO
getForce :: ∀ eff. String -> D3Simulation                         -> Effect D3Force
getForce name sim = runEffectFn2 getForceFn name sim

-- || functions only for LINK force
makeLinkForce :: ∀ eff. Maybe (Array Link) -> Effect D3Force
makeLinkForce (Just ls) = runEffectFn1 makeLinkForceFn   ls
makeLinkForce Nothing   = runEffectFn1 makeLinkForceFn   []

setIDFunction :: ∀ v eff. (Node -> Index -> v) -> D3Force -> Effect D3Force -- Force HAS TO BE LINK FORCE HERE
setIDFunction f = runEffectFn2 linkIDFn (mkFn2 f)

setLinks      :: ∀ eff. (Array Link) -> D3Force -> Effect D3Force
setLinks      = runEffectFn2 setLinksFn

getLinks      :: ∀ eff. D3Simulation            -> Effect (Array Link)
getLinks      = runEffectFn1 getLinksFn

-- || functions only for MANY BODY force
makeManyBody :: ∀ eff. Effect D3Force
makeManyBody = makeManyBodyForceFn

-- || functions only for CENTERING force was using Data.Pair to be a little more
-- precise but dropped this to get it to compile with psc-package which doesn't
-- yet have that
makeCenterForce :: ∀ eff. Maybe (Array Number) -> Effect D3Force
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
