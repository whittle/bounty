import Paths_bounty (version)
import qualified Bounty.Application as B
import Bounty.Options (Settings(..), options)
import Data.Conduit
import qualified Data.Conduit.Network as N
import qualified Options.Applicative as O

main :: IO ()
main = do
  settings <- O.execParser options
  state <- B.initState
  N.runTCPServer (server settings) $ app state

app :: B.State -> N.Application IO
app s d = N.appSource d $= B.application appData s $$ N.appSink d

appData :: B.AppData
appData = B.AppData version

server :: Settings -> N.ServerSettings IO
server s = N.serverSettings (port s) $ hostPreference s
