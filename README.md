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

* Front-end lexer created by [Alex](http://www.haskell.org/alex/)
  (like flex for Haskell).

* Front-end parser created by [Happy](http://www.haskell.org/happy/)
  (like yacc for Haskell).

* Underlying data structure is just an alist while I get the interface
  working. Obviously, that is not optimal.
