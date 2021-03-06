# ChangeLog / ReleaseNotes


## Version 0.2.3 (unreleased)

* Introducing flag `examples` for building example executables.


## Version 0.2.2

* Removed deprecation warnings about the use of `sClose` instead of `close`.
  (**change**)
* Bumped upper bound on `streaming-commons` to include 0.2.\* versions.


## Version 0.2.1

* Relaxed upper bound on base to include version 4.9.\*.
* Relaxed bounds for [data-default-class][] to include 0.1.\*.
* Getting rid of compilation warnings on GHC 8 and when compiled with pedantic
  flag.
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/connection-pool-0.2.1>


## Version 0.2

* Release has backward compatible API with 0.1 branch.
* Introducing `ConnectionPoolFor` type class which has instances for both
  `ConnectionPool TcpClient` and `ConnectionPool UnixClient`. Class is located
  in its own module `Data.ConnectionPool.Class`, therefore it is part of stable
  API. It provides `withConnection` and `destroyAllConnections` methods which
  can be used instead of their more specific equivalents. (**new**)
* `ConnectionPool` data family moved in to its own module
  `Data.ConnectionPool.Family`, as a consequence it became part of stable API.
  (**change**)
* Introducing `tryWithUnixClientConnection` and `tryWithTcpClientConnection`
  functions. (**new**)
* Providing instances of `Generic` and `Show` where ever possible and
  reasonable. This is a backwards compatible change. (**new**)
* Internal `ConnectionPool` data type is now more generic because `Socket`
  handle isn't hard-coded in it any more. This change breaks packages depending
  on internal API. (**change**)
* Internal type class `HasConnectionPool` was introduced to simplify access to
  `ConnectionPool` data type wrapped in other types. (**new**)
* Internal modules were heavily reorganized and TCP and UNIX Sockets related
  implementations were moved in to their own modules. This change breaks
  packages depending on internal API. (**change**)
* Heavy inlining of everything. Purpose is to be safe that this library gets
  abstracted away as much as possible. Best result is if only direct references
  to [resource-pool][] and [streaming-commons][] remain. (**change**)
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/connection-pool-0.2>


## Version 0.1.3

* All lenses are now defined as strict, as a consequence lower bound of
  [between][] is now `0.10.0.0` instead of `0.9.0.0`. (**change**)
* Support for user defined read buffer size, this was introduced in
  [streaming-commons][] `== 0.1.13`. Non-internal library API is backwards
  compatible. (**new**)
* Default buffer size changed in [streaming-commons][] `== 0.1.13` to 32kiB,
  this library uses this value as a default even if it's built with
  [streaming-commons][] `< 0.1.13`. For more details see
  <https://github.com/fpco/streaming-commons/issues/22>. (**change**)
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/connection-pool-0.1.3>


## Version 0.1.2.1

* Builds also with [streaming-commons][] `>0.1.5 && <0.1.14`. Tested up to
  [streaming-commons][] version 0.1.13. See also issue #1
  <https://github.com/trskop/connection-pool/issues/1> (**bugfix**)
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/connection-pool-0.1.2.1>


## Version 0.1.2.0

* Builds with GHC 7.10 and base 4.8. (**new**)
* Builds also with [streaming-commons][] `>0.1.5 && <0.1.13`. Tested up to
  [streaming-commons][] version 0.1.12.1. (**new**)
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/connection-pool-0.1.2.0>


## Version 0.1.1.0

* Package is now buildable on Windows. (**new**)
* Introducing function `validateResourcePoolParams`. (**new**)
* Introducing internal function `destroyAllConnections`. (**new**)
* Introducing functions `destroyAllTcpClientConnections` and
  `destroyAllTcpClientConnections` both build on top of
  `destroyAllConnections`. (**new**)
* Corrected some typos in documentation and Haddock markup.
* Small documentation enhancements.
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/connection-pool-0.1.1.0>


## Version 0.1.0.0

* First public release.
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/connection-pool-0.1.0.0>



[between]:
  http://hackage.haskell.org/package/between
  "Function combinator 'between' and derived combinators."
[data-default-class]:
  http://hackage.haskell.org/package/data-default-class
  "Default type class provides a default value (def) of a type."
[Hackage]:
  http://hackage.haskell.org/
  "HackageDB (or just Hackage) is a collection of releases of Haskell packages."
[resource-pool]:
  http://hackage.haskell.org/package/resource-pool
  "A high-performance striped pooling abstraction for managing resources."
[streaming-commons]:
  http://hackage.haskell.org/package/streaming-commons
  "Low-dependency functionality commonly needed by various streaming data libraries"
