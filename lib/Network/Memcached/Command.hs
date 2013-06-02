{-# LANGUAGE OverloadedStrings #-}
module Network.Memcached.Command
       ( Command(..)
       , StatisticsOption(..)
       , apply
       , isQuit
       , showMsg
       ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Map ((!))
import Network.Memcached.Types

-- | All the different commands that regular memcached accepts.
data Command = SetCommand Key Flags Exptime Bytes Reply
             | AddCommand Key Flags Exptime Bytes Reply
             | ReplaceCommand Key Flags Exptime Bytes Reply
             | AppendCommand Key Flags Exptime Bytes Reply
             | PrependCommand Key Flags Exptime Bytes Reply
             | CasCommand Key Flags Exptime Bytes CasUnique Reply
             | DeleteCommand Key (Maybe Time) Reply
             | IncrementCommand Key Integer Reply
             | DecrementCommand Key Integer Reply
             | FlushAllCommand (Maybe Integer) Reply
             | GetCommand [Key]
             | GetsCommand [Key]
             | StatisticsCommand (Maybe StatisticsOption)
             | VersionCommand
             | VerbosityCommand VerbosityLevel
             | QuitCommand
             deriving (Eq, Show)

apply :: Command -> MemState -> (String, MemState)
apply (SetCommand k _ _ b _) s = ("Set " ++ BS.unpack k ++ " to " ++ show b, Map.insert k b s)
apply (GetCommand ks) s = (showReverse s ks, s)
apply _ s = ("No action taken: " ++ show s, s)

showReverse :: MemState -> [Key] -> String
showReverse m = unwords . reverse . map (\k -> show (m ! k))

isQuit :: Command -> Bool
isQuit QuitCommand = True
isQuit _ = False

showMsg :: Command -> Bool
showMsg (SetCommand _ _ _ _ False) = False
showMsg QuitCommand = False
showMsg _ = True
