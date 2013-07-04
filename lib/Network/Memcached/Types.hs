module Network.Memcached.Types
       ( AppData(..)
       , State
       , Mem
       , Record(..)
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
import Data.Time.Clock.POSIX
import Data.Version (Version)
import Data.Word

data AppData = AppData { appVersion :: Version
                       } deriving (Eq, Show)

type State = TVar Mem
type Mem = Map.Map Key Record
data Record = Record { flags :: Flags
                     , exptime :: Maybe POSIXTime
                     , unique :: CasUnique
                     , content :: Content
                     } deriving (Eq, Show)

type Key = B.ByteString
type Flags = Word16
type Exptime = Word32
type Bytes = Int
type Reply = Bool
type Content = B.ByteString
type CasUnique = Word64
type Time = Integer
type VerbosityLevel = Integer

-- | The stats command can accept one of four different options.
data StatisticsOption = StatSettings
                      | StatItems
                      | StatSlabs
                      | StatSizes
                      deriving (Eq, Show)
