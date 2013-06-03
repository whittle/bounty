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
data Command = Set Key Flags Exptime Reply Content
             | Add Key Flags Exptime Reply Content
             | Replace Key Flags Exptime Reply Content
             | Append Key Flags Exptime Reply Content
             | Prepend Key Flags Exptime Reply Content
             | Cas Key Flags Exptime CasUnique Reply Content
             | Get [Key]
             | Delete Key Reply
             | Increment Key Integer Reply
             | Decrement Key Integer Reply
             | Touch Key Exptime Reply
             | SlabsReassign Integer Integer
             | SlabsAutomove Integer
             | Statistics (Maybe StatisticsOption)
             | FlushAll (Maybe Integer) Reply
             | Version
             | Verbosity VerbosityLevel
             | Quit
             deriving (Eq, Show)

apply :: Command -> TVar MemState -> IO (Maybe B.ByteString)
apply (Set k _ _ r c) tm = do
  atomically $ do
    m <- readTVar tm
    writeTVar tm $ Map.insert k c m
  return $ if r
           then Just "STORED\r\n"
           else Nothing
apply (Add k _ _ r c) tm = do
  msg <- atomically $ do
    m <- readTVar tm
    if Map.member k m
      then return "NOT_STORED\r\n"
      else do writeTVar tm $ Map.insert k c m
              return "STORED\r\n"
  return $ if r then Just msg else Nothing
apply (Replace k _ _ r c) tm = do
  msg <- atomically $ do
    m <- readTVar tm
    if Map.notMember k m
      then return "NOT_STORED\r\n"
      else do writeTVar tm $ Map.insert k c m
              return "STORED\r\n"
  return $ if r then Just msg else Nothing
apply (Append k _ _ r c) tm = do
  msg <- atomically $ do
    m <- readTVar tm
    if Map.notMember k m
      then return "NOT_STORED\r\n"
      else do writeTVar tm $ Map.insertWith (flip B.append) k c m
              return "STORED\r\n"
  return $ if r then Just msg else Nothing
apply (Prepend k _ _ r c) tm = do
  msg <- atomically $ do
    m <- readTVar tm
    if Map.notMember k m
      then return "NOT_STORED\r\n"
      else do writeTVar tm $ Map.insertWith B.append k c m
              return "STORED\r\n"
  return $ if r then Just msg else Nothing
apply (Get ks) tm = do
  m <- readTVarIO tm
  return $ Just $ B.pack $ unwords (map (\k -> show (m ! k)) ks) ++ "\r\n"
apply (Delete k r) tm = do
  msg <- atomically $ do
    m <- readTVar tm
    if Map.notMember k m
      then return "NOT_FOUND\r\n"
      else do writeTVar tm $ Map.delete k m
              return "DELETED\r\n"
  return $ if r then Just msg else Nothing
apply (Increment k _ r) tm = do
  msg <- atomically $ do
    m <- readTVar tm
    undefined
  undefined
apply Quit _ = return Nothing
apply s _ = return $ Just $ B.pack $ "No action taken: " ++ show s ++ "\r\n"

isQuit :: Command -> Bool
isQuit Quit = True
isQuit _ = False
