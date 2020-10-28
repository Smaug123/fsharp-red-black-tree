# Red-black tree

This is a basic implementation of a red-black tree in F#, which is pretty close to being correct by construction.

## Things to fix

* Can we do better than the awful recursion to `elevateBlack` and `elevateRed`? That is, can we make the type parameter truly phantom, erased at runtime?
* Can we get some constraints at the type level to help us get the values assigned to the various nodes in the right order?
* Can we get some kind of better proof that we are not violating the invariants? There's nothing to help us get the `elevate` calls right.
