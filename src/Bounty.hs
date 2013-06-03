import Paths_bounty (version)
import qualified Bounty.Application as B
import Data.Conduit
import qualified Data.Conduit.Network as N

main :: IO ()
main = do
  s <- B.initState
  N.runTCPServer settings $ app s

app :: B.State -> N.Application IO
app s d = (N.appSource d) $= B.application appData s $$ (N.appSink d)

appData :: B.AppData
appData = B.AppData version

settings :: N.ServerSettings IO
settings = N.serverSettings 22122 N.HostAny
