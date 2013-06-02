Bounty
======

Bounty is a drop-in replacement for memcached that also supports a
subset of the relational algebra. Inspired by [Neha
Narula](http://nehanaru.la/)’s presentation about
[Pequod](http://lanyrd.com/2013/riconeast/scgcrq/) at [Ricon
East](http://ricon.io/east.html) 2013.

__N.B.__ Bounty is currently highly experimental, and not even kind of
complete.


Implementation
--------------

* Implemented in
  [Haskell](http://www.haskell.org/haskellwiki/Haskell).

* The front-end parser is build in
  [Attoparsec](http://hackage.haskell.org/package/attoparsec) for
  speed. As much as possible is done using
  [bytestrings](http://hackage.haskell.org/package/bytestring), again
  for speed.

* The underlying data structure is just a
  [Map](http://hackage.haskell.org/packages/archive/containers/latest/doc/html/Data-Map.html)
  while I get the interface working. Obviously, this needs to be
  replaced with something more concurrency-friendly once things get
  going.
