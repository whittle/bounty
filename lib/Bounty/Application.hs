module Bounty.Application
       ( newState
       , application
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

newState :: IO (TVar MemState)
newState = atomically $ newTVar Map.empty

application :: TVar MemState -> Conduit B.ByteString IO B.ByteString
application tm = parser =$= quitter =$= applier tm =$= CL.catMaybes

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

applier :: TVar MemState -> Conduit CommandParse IO (Maybe B.ByteString)
applier tm = CL.mapM applier'
  where applier' (Left e) = return $ Just $ B.pack $ "CLIENT_ERROR " ++ show e ++ "\r\n"
        applier' (Right (_, c)) = apply c tm
