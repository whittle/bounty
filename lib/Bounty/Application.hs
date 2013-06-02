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
application tm = parser =$= shower tm

type CommandParse = Either ParseError (PositionRange, Command)

parser :: Conduit B.ByteString IO CommandParse
parser = conduitParserEither command

shower :: TVar MemState -> Conduit CommandParse IO B.ByteString
shower tm = CL.mapM $ applier tm

applier :: TVar MemState -> CommandParse -> IO B.ByteString
applier _ (Left e) = return $ B.pack $ show e ++ "\r\n"
applier tm (Right (_, c)) = apply c tm
