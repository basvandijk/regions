This package provides the region monad transformer. Scarce resources
like files, memory pointers or USB devices for example can be opened
in a region. When the region terminates, all opened resources will be
automatically closed. The main advantage of regions is that the opened
resources can not be returned from the region which ensures no I/O
with closed resources is possible.

The primary technique used in this package is called "Lightweight
monadic regions" which was [invented][1] by Oleg Kiselyov and
Chung-chieh Shan.

Also see the [regions-mtl] and [regions-monadstf] packages which
provide instances for the classes in the respected monad transformers
packages.

For an example on how to use this library see the
[safer-file-handles], [usb-safe] or [regional-pointers] packages.

[1]: http://okmij.org/ftp/Haskell/regions.html#light-weight

[regions-mtl]:        http://hackage.haskell.org/package/regions-mtl
[regions-monadstf]:   http://hackage.haskell.org/package/regions-monadstf
[safer-file-handles]: http://hackage.haskell.org/package/safer-file-handles
[usb-safe]:           http://hackage.haskell.org/package/usb-safe
[regional-pointers]:  http://hackage.haskell.org/package/regional-pointers
