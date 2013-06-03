module Bounty.Application
       ( initState
       , application
       , AppData(..)
       , State
       ) where

import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as B
import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Conduit.List as CL
import qualified Data.Map as Map
import Network.Memcached.Command
import Network.Memcached.Parser
import Network.Memcached.Types

initState :: IO State
initState = atomically $ newTVar Map.empty

application :: AppData -> State -> Conduit B.ByteString IO B.ByteString
application d s = parser =$= quitter =$= applier d s =$= CL.catMaybes

type CommandParse = Either ParseError (PositionRange, Command)

parser :: Conduit B.ByteString IO CommandParse
parser = conduitParserEither command

quitter :: Conduit CommandParse IO CommandParse
quitter = do
  mcp <- await
  case mcp of
    Nothing -> return ()
    Just (Right (_, Quit)) -> return ()
    Just cp -> yield cp >> quitter

applier :: AppData -> State -> Conduit CommandParse IO (Maybe B.ByteString)
applier d s = CL.mapM applier'
  where applier' (Left e) = return $ Just $ B.pack $ "CLIENT_ERROR " ++ show e ++ "\r\n"
        applier' (Right (_, c)) = apply c d s
