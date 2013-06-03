{-# LANGUAGE OverloadedStrings #-}
module Network.Memcached.Command
       ( Command(..)
       , apply
       , isQuit
       ) where

import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import Data.Map ((!))
import Network.Memcached.Types

-- | All the different commands that regular memcached accepts.
data Command = SetCommand Key Flags Exptime Reply Content
             | AddCommand Key Flags Exptime Reply Content
             | ReplaceCommand Key Flags Exptime Reply Content
             | AppendCommand Key Flags Exptime Reply Content
             | PrependCommand Key Flags Exptime Reply Content
             | CasCommand Key Flags Exptime CasUnique Reply Content
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

apply :: Command -> TVar MemState -> IO (Maybe B.ByteString)
apply (SetCommand k _ _ r c) tm = do
  atomically $ do
    m <- readTVar tm
    writeTVar tm $ Map.insert k c m
  return $ if r
           then Just $ B.pack $ "Set " ++ B.unpack k ++ " to " ++ show c ++ "\r\n"
           else Nothing
apply (GetCommand ks) tm = do
  m <- readTVarIO tm
  return $ Just $ B.pack $ (unwords $ map (\k -> show (m ! k)) ks) ++ "\r\n"
apply QuitCommand _ = return Nothing
apply s _ = return $ Just $ B.pack $ "No action taken: " ++ show s ++ "\r\n"

isQuit :: Command -> Bool
isQuit QuitCommand = True
isQuit _ = False
