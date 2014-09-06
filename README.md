Connection poll is a family specialised resource pools. Currently package
provides two

1. pool for TCP client connections,

2. and pool for UNIX Sockets client connections.

This package is built on top of [resource-pool][] and [streaming-commons][]
packages. The later allows us to use [conduit-extra][] package for
implementation of TCP or UNIX Sockets clients.


[conduit-extra]:
  http://hackage.haskell.org/package/conduit-extra
[resource-pool]:
  http://hackage.haskell.org/package/resource-pool
[streaming-commons]:
  http://hackage.haskell.org/package/streaming-commons
