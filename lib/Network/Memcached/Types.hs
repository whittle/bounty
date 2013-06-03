module Network.Memcached.Types where

import Control.Monad.Trans.State
import qualified Data.ByteString as B
import qualified Data.Map as Map

type MemState = Map.Map Key Content
type MemStateM a = StateT MemState IO a

type Key = B.ByteString
type Flags = Integer
type Exptime = Integer
type Bytes = Int
type Reply = Bool
type Content = B.ByteString
type CasUnique = Integer
type Time = Integer
type VerbosityLevel = Integer

-- | The StatisticsCommand can accept one of three different options.
data StatisticsOption = StatisticsOptionSettings
                      | StatisticsOptionItems
                      | StatisticsOptionSlabs
                      | StatisticsOptionSizes
                      deriving (Eq, Show)
