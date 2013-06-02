module Bounty.Application
       ( application
       ) where

import qualified Data.ByteString.Char8 as B
import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Conduit.List as CL
import Network.Memcached.Parser
import Network.Memcached.Command

application :: MonadThrow m => Conduit B.ByteString m B.ByteString
application = parser =$= shower

type CommandParse = Either ParseError (PositionRange, Command)

parser :: MonadThrow m => Conduit B.ByteString m CommandParse
parser = conduitParserEither command

shower :: Monad m => Conduit CommandParse m B.ByteString
shower = CL.map $ B.pack . (++"\n") . show
