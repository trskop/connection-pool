Connection Pool
===============

[![Hackage](http://img.shields.io/hackage/v/connection-pool.svg)
][Hackage: connection-pool]
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]

[![Build](https://travis-ci.org/trskop/connection-pool.svg)](https://travis-ci.org/trskop/connection-pool)


Description
-----------

Connection poll is a family specialised resource pools. Currently package
provides two

1. pool for TCP client connections,
2. and pool for UNIX Sockets client connections.

This package is built on top of [resource-pool][Hackage: resource-pool] and
[streaming-commons][Hackage: streaming-commons]. The later allows us to use
[conduit-extra][Hackage: conduit-extra] package for implementation of TCP or
UNIX Sockets clients.


Documentation
-------------

Stable releases with API documentation are available on
[Hackage][Hackage: connection-pool]


Examples
--------

Simple code examples, including example from the following section, are
available in [example/](example/) directory.


TCP Client Example
------------------

Here is a simple example that demonstrates how TCP client can be created and
how connection pool behaves.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main (main)
  where

import Control.Monad (void)
import Control.Concurrent (forkIO, threadDelay)
import System.Environment (getArgs)

import Control.Lens ((.~), (&))
import Data.ConnectionPool
    ( createTcpClientPool
    , numberOfResourcesPerStripe
    , numberOfStripes
    , withTcpClientConnection
    )
import Data.Default.Class (Default(def))
import Data.Streaming.Network (appWrite, clientSettingsTCP)


main :: IO ()
main = do
    [port, numStripes, numPerStripe] <- getArgs
    pool <- createTcpClientPool
        (poolParams numStripes numPerStripe)
        (clientSettingsTCP (read port) "127.0.0.1")
    void . forkIO . withTcpClientConnection pool $ \appData -> do
       threadDelay 100
       appWrite appData "1: I'm alive!\n"
    void . forkIO . withTcpClientConnection pool $ \appData ->
       appWrite appData "2: I'm alive!\n"
  where
    poolParams m n =
        def & numberOfStripes .~ read m
            & numberOfResourcesPerStripe .~ read n
```

To test it we can use `socat` or some `netcat` like application. Our test will
require two terminals, in one we will execute `socat` as a server listenting on
UNIX socket and in the other one we execute above example.

Simple TCP server listening on port `8001` that prints what it receives to
stdout:

    $ socat TCP4-LISTEN:8001,bind=127.0.0.1,fork -

The `fork` parameter in the above example is important, otherwise `socat` would
terminate when client closes its connection.

If we run above example as:

    $ runghc tcp-example.hs 8001 1 1

We can see that `socat` received following text:

    1: I'm alive!
    2: I'm alive!

But if we increment number of stripes or number of connections (resources) per
stripe, then we will get:

    2: I'm alive!
    1: I'm alive!

The reason for this is that we use `threadDelay 100` in the first executed
thread. So when we have only one stripe and one connection per stripe, then we
have only one connection in the pool. Therefore when the first thread executes
and acquires a connection, then all the other threads (the other one in above
example) will block. If we have more then one connection available in our pool,
then the first thread acquires connection, blocks on `threadDelay` call, but
the other thread also acquires connection and prints its output while the first
thread is still blocked on `threadDelay`. This example demonstrates how
connection pool behaves if it reached its capacity and when it has onough free
resources.


License
-------

The BSD 3-Clause License, see [LICENSE](LICENSE) file for details.


Contributions
-------------

Contributions, pull requests and bug reports are welcome! Please don't be
afraid to contact author using GitHub or by e-mail (see `.cabal` file for
that).



[Hackage: conduit-extra]:
  http://hackage.haskell.org/package/conduit-extra
[Hackage: connection-pool]:
  http://hackage.haskell.org/package/connection-pool
[Hackage: resource-pool]:
  http://hackage.haskell.org/package/resource-pool
[Hackage: streaming-commons]:
  http://hackage.haskell.org/package/streaming-commons
