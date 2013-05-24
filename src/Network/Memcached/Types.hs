module Network.Memcached.Types where

import Control.Monad.Trans.State
import qualified Data.Map as Map

type MemState = Map.Map Key Bytes
type MemStateM a = StateT MemState IO a

type Key = String
type Flags = Integer
type Exptime = Integer
type Bytes = Integer
type CasUnique = Integer
type Time = Integer
type VerbosityLevel = Integer
