{-# LANGUAGE OverloadedStrings #-}
module Network.Memcached.Command
       ( Command(..)
       , apply
       , isQuit
       , showMsg
       ) where

import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as B
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

apply :: Command -> TVar MemState -> IO (Maybe B.ByteString)
apply (SetCommand k _ _ b True) tm = do
  atomically $ do
    m <- readTVar tm
    writeTVar tm $ Map.insert k b m
  return $ Just $ B.pack $ "Set " ++ B.unpack k ++ " to " ++ show b ++ "\r\n"
apply (SetCommand k _ _ b False) tm = do
  atomically $ do
    m <- readTVar tm
    writeTVar tm $ Map.insert k b m
  return Nothing
apply (GetCommand ks) tm = do
  m <- readTVarIO tm
  return $ Just $ B.pack $ (unwords $ map (\k -> show (m ! k)) ks) ++ "\r\n"
apply QuitCommand _ = return Nothing
apply s _ = return $ Just $ B.pack $ "No action taken: " ++ show s ++ "\r\n"

isQuit :: Command -> Bool
isQuit QuitCommand = True
isQuit _ = False

showMsg :: Command -> Bool
showMsg (SetCommand _ _ _ _ False) = False
showMsg QuitCommand = False
showMsg _ = True
