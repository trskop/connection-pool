# ChangeLog / ReleaseNotes

## Version 0.1.2.0

* Builds with GHC 7.10 and base 4.8. (new)
* Builds with also with [streaming-commons][] `>1.5 && <1.13`. Tested up to
  version 1.12.1. (new)
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/connection-pool-0.1.2.0>


## Version 0.1.1.0

* Package is now buildable on Windows. (new)
* Introducing function `validateResourcePoolParams`. (new)
* Introducing internal function `destroyAllConnections`. (new)
* Introducing functions `destroyAllTcpClientConnections` and
  `destroyAllTcpClientConnections` both build on top of
  `destroyAllConnections`. (new)
* Corrected some typos in documentation and Haddock markup.
* Small documentation enhancements.
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/connection-pool-0.1.1.0>


## Version 0.1.0.0

* First public release.
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/connection-pool-0.1.0.0>



[Hackage]:
  http://hackage.haskell.org/
  "HackageDB (or just Hackage) is a collection of releases of Haskell packages."
[streaming-commons]:
  http://hackage.haskell.org/package/streaming-commons
  "Low-dependency functionality commonly needed by various streaming data libraries"

<!--
  vim: filetype=markdown softtabstop=4 shiftwidth=4 expandtab
-->
