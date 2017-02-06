# purescript-d3-tagless

Applying the "finally tagless, partially evaluated" style to a D3 eDSL

See README for my other repo https://github.com/afcondon/purescript-d3v4 for background on D3 in Purescript

See README for this repo https://github.com/afcondon/purescript-finally-tagless-ex for references on
finally tagless DSLs in Purescript

TL;DR wrapping the D3 JavaScript API slavishly has drawbacks, but making an interpreted
language leads to lots of visual noise due to tag syntax. Adopting finally tagless approach promises to be really
nice but it's a work in progress.


## Note

I think it's impossible to express all of D3 in a typed way without adding
unacceptable amounts of syntactic noise. However, the tagless approach would
allow for us to write some syntactic checking at some point so that a kind of
`lint` could be provided for D3 scripts w/o introducing the overhead of
additional syntax
