import Data.Conduit
import Data.Conduit.Network

import Bounty.Application

main :: IO ()
main = runTCPServer settings $ \c -> (appSource c) $= application $$ (appSink c)

settings :: ServerSettings IO
settings = serverSettings 22122 HostAny
