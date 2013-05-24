module Network.Memcached.Command
       ( Command(..)
       , StatisticsOption(..)
       , apply
       , isQuit
       , showMsg
       ) where

import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Map ((!))
import Network.Memcached.Types

-- | All the different commands that regular memcached accepts.
data Command = SetCommand Key Flags Exptime Bytes Bool
             | AddCommand Key Flags Exptime Bytes Bool
             | ReplaceCommand Key Flags Exptime Bytes Bool
             | AppendCommand Key Flags Exptime Bytes Bool
             | PrependCommand Key Flags Exptime Bytes Bool
             | CasCommand Key Flags Exptime Bytes CasUnique Bool
             | DeleteCommand Key (Maybe Time) Bool
             | IncrementCommand Key Integer Bool
             | DecrementCommand Key Integer Bool
             | FlushAllCommand (Maybe Integer) Bool
             | GetCommand [Key]
             | GetsCommand [Key]
             | StatisticsCommand (Maybe StatisticsOption)
             | VersionCommand
             | VerbosityCommand VerbosityLevel
             | QuitCommand
             deriving Show

apply :: Command -> MemState -> (String, MemState)
apply (SetCommand k _ _ b _) s = ("Set " ++ k ++ " to " ++ show b, Map.insert k b s)
apply (GetCommand ks) s = (showReverse s ks, s)
apply _ s = ("No action taken: " ++ show s, s)

showReverse :: MemState -> [Key] -> String
showReverse m = intercalate " " . reverse . map (\k -> show (m ! k))

isQuit :: Command -> Bool
isQuit QuitCommand = True
isQuit _ = False

showMsg :: Command -> Bool
showMsg (SetCommand _ _ _ _ False) = False
showMsg QuitCommand = False
showMsg _ = True

-- | The StatisticsCommand can accept one of three different options.
data StatisticsOption = StatisticsOptionItems
                      | StatisticsOptionSlabs
                      | StatisticsOptionSizes
                      deriving Show
