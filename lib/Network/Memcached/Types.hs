module Network.Memcached.Types
       ( AppData(..)
       , State
       , Mem
       , Key
       , Flags
       , Exptime
       , Bytes
       , Reply
       , Content
       , CasUnique
       , Time
       , VerbosityLevel
       , StatisticsOption(..)
       ) where

import Control.Concurrent.STM (TVar)
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Version (Version)

data AppData = AppData { appVersion :: Version
                       } deriving (Eq, Show)

type State = TVar Mem
type Mem = Map.Map Key Content

type Key = B.ByteString
type Flags = Integer
type Exptime = Integer
type Bytes = Int
type Reply = Bool
type Content = B.ByteString
type CasUnique = Integer
type Time = Integer
type VerbosityLevel = Integer

-- | The stats command can accept one of four different options.
data StatisticsOption = StatSettings
                      | StatItems
                      | StatSlabs
                      | StatSizes
                      deriving (Eq, Show)
