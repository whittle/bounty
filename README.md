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

* All IO is handled via
  [conduits](http://hackage.haskell.org/package/conduit) for constant
  resource usage. The TCP server uses
  [network-conduit](http://hackage.haskell.org/package/network-conduit)
  for the same reason. UDP and Unix-domain socket styles are planned.

* The underlying data structure is just a
  [Map](http://hackage.haskell.org/packages/archive/containers/latest/doc/html/Data-Map.html)
  while I get the interface working. Obviously, this needs to be
  replaced with something more concurrency-friendly once things get
  going.

* Tests are all written in HUnit for the moment.

Notes
-----

* After researching a number of different options, I’m exploring the
  possibility of using
  [Prefix Hash Trees (PHTs)](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.58.617)
  for distributed range queries. Based on that paper, I’m implementing
  this in a separate library.

* I keep getting this feeling like Knuth has already solved this, but
  I can’t find it. Maybe
  [Volume 5](https://en.wikipedia.org/wiki/The_Art_of_Computer_Programming).

Known Issues
------------

* There are some space leaks around pushing data onto the STM Map in a
  naïve fashion. If the final product was going to store everything in
  a lazy btree wrapped in a TVar, I would be remiss for not explicitly
  copying key and value bytestrings and evaluating the btree to normal
  form prior to shoving it back into STM. Instead, I’m already
  researching more concurrency-friendly data structures (see Notes).

* Some inputs can totally crash the server. Ouch. That’s gonna need
  some exception handling. I’ll add that when I reorganize how
  commands are implemented (it’s ugly right now).
