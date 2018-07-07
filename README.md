# purescript-annoy

PureScript bindings for [annoy-node](https://github.com/jimkang/annoy-node)

Please refer to [original annoy](https://github.com/spotify/annoy) for any additional information

[Docs published on Pursuit](https://pursuit.purescript.org/packages/purescript-annoy)

## Modules:
- **Annoy.Unsafe** - Direct bindings, foundation for other modules. It is better to avoid using functions from this module.
- **Annoy.ST** - Building Annoy in a mutable way.
- **Annoy** - Main functionality + building Annoy from pure collection of vectors


## Task List
- [ ] build + getnns nondeterministic?
- [ ] P2: len reference in STAnnoy
- [x] all pure annoy api
- [x] docs
- [x] f { trees, size, metric } vs f trees size metric
- [x] metric as type
- [x] Nat vs Int. Both
- [x] parameter order, first annoy
- [x] publish
- [x] safe: create from monadic action
- [x] safe: new with 2D array
- [x] safe: vectors with typelevel length 
- [x] test: Integration
- [x] unsafe: include distances


## Credits

Library is co-created and funded by [λ-terms](https://github.com/lambdaterms/)
