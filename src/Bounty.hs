import Data.Conduit
import Data.Conduit.Network

import Bounty.Application

main :: IO ()
main = do
  mem <- newState
  runTCPServer settings $ \c -> (appSource c) $= application mem $$ (appSink c)

settings :: ServerSettings IO
settings = serverSettings 22122 HostAny
