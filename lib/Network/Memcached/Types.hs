module Network.Memcached.Types where

import Control.Monad.Trans.State
import qualified Data.ByteString as BS
import qualified Data.Map as Map

type MemState = Map.Map Key Bytes
type MemStateM a = StateT MemState IO a

type Key = BS.ByteString
type Flags = Integer
type Exptime = Integer
type Bytes = Integer
type Reply = Bool
type Content = BS.ByteString
type CasUnique = Integer
type Time = Integer
type VerbosityLevel = Integer

-- | The StatisticsCommand can accept one of three different options.
data StatisticsOption = StatisticsOptionItems
                      | StatisticsOptionSlabs
                      | StatisticsOptionSizes
                      deriving (Eq, Show)
