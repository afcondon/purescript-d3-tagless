# Design Notes for Finally Tagless D3

## rationale for the D3 primitives

It seems at least possible that a richer API can be built for - or layer over - a faithful copy of D3's own API endpoints.

### example of multiple layers of Selection
```
For example, to select the bold elements in every paragraph:

var b = d3.selectAll("p").selectAll("b");
```

## apparent impossibility of typing transition and selection as separate in any way

In order to chain a transition onto a selection-y monad they have to be the same monad but then you lose the ability to distinguish them as separate types. Perhaps this can be solved with some kind of newtyping but i don't know how to do this in the finally tagless interpreter

## example of a callback function on some more structured data

The bad thing here is that we have lose the typing information in the FFI, so we can give a callback that does not match the data we are using. Here's an example of where this is both bad (lost our static typing!) and good (PureScript makes it so nice and clear to right grouping functions).
```
-- an example of some structured data that we are going to write a callback for
type ExData = { name :: String, age :: Int }

-- lp :: ExData -> Number -> Array D3Element -> D3Element -> Int
lp :: AttrSetter ExData Int
lp { name, age } _ _ _ =
    case name, age of -- silly little function just shows one way you might use an index function (NB in many cases D3 has better solutions for grouping)
    "awn", _ -> 20
    _, 0     -> 50
    _, _     -> 100
```

## when you don't have an index function for the data

In this case you have a universal type variable in the signature for the data, like so:

```
myData :: ∀ i. D3Data Int i -- there is no index fn, so 'i' type parameter is open / universal
myData       = ArrayD [1,2,3,4,5]
```

## Idempotency of d3.transtion()

this next relies on D3 to return us the same transition as "name" (ie if named
transition exists) allowing us to call this function multiple times from
different scripts that are under interpretation

now, if we want to add (common) attributes to this transition definition we'll
have to check if it already exists and not apply those attributes for existing
transitions

```
myTransition :: D3Script
myTransition = do
    makeTransition $ TransitionName "foo"
    delay $ MilliSec 2000.0
```

## higher level functions

It's so common to do `select...data...enter...append` that you could define a shorthand for this (and maybe we will). the functions are separate in D3 because it's also quite common to want to do `select...select...data...enter...append` or to replace the `append` with an `insert` etc. the challenge here is to find ways of nesting this language or parameterizing the action (with append or insert)...

I do not know of any situation where you want to separate the `data` and `enter` but presumably there is one? 
