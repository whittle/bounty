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
import Data.Monoid ((<>))
import Data.Version (showVersion)
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

apply :: Command -> AppData -> State -> IO (Maybe B.ByteString)
apply (Set k _ _ r c) _ tm = do
  atomically $ do
    m <- readTVar tm
    writeTVar tm $ Map.insert k c m
  return $ if r
           then Just "STORED\r\n"
           else Nothing
apply (Add k _ _ r c) _ tm = do
  msg <- atomically $ do
    m <- readTVar tm
    if Map.member k m
      then return "NOT_STORED\r\n"
      else do writeTVar tm $ Map.insert k c m
              return "STORED\r\n"
  return $ if r then Just msg else Nothing
apply (Replace k _ _ r c) _ tm = do
  msg <- atomically $ do
    m <- readTVar tm
    if Map.notMember k m
      then return "NOT_STORED\r\n"
      else do writeTVar tm $ Map.insert k c m
              return "STORED\r\n"
  return $ if r then Just msg else Nothing
apply (Append k _ _ r c) _ tm = do
  msg <- atomically $ do
    m <- readTVar tm
    if Map.notMember k m
      then return "NOT_STORED\r\n"
      else do writeTVar tm $ Map.insertWith (flip B.append) k c m
              return "STORED\r\n"
  return $ if r then Just msg else Nothing
apply (Prepend k _ _ r c) _ tm = do
  msg <- atomically $ do
    m <- readTVar tm
    if Map.notMember k m
      then return "NOT_STORED\r\n"
      else do writeTVar tm $ Map.insertWith B.append k c m
              return "STORED\r\n"
  return $ if r then Just msg else Nothing
apply (Get ks) _ tm = do
  m <- readTVarIO tm
  let print'' k v = "VALUE " <> k <> " 0 " <> (B.pack $ show $ B.length v) <> "\r\n" <> v <> "\r\n"
  let print' k = if Map.member k m
                 then print'' k $ m ! k
                 else ""
  let vals = map print' ks
  return $ Just $ B.concat vals <> "END\r\n"
apply (Delete k r) _ tm = do
  msg <- atomically $ do
    m <- readTVar tm
    if Map.notMember k m
      then return "NOT_FOUND\r\n"
      else do writeTVar tm $ Map.delete k m
              return "DELETED\r\n"
  return $ if r then Just msg else Nothing
apply (Increment _k _ _r) _ tm = do
  _msg <- atomically $ do
    _m <- readTVar tm
    undefined
  undefined
apply Version d _ = return $ Just $ "VERSION " `B.append` (B.pack $ showVersion $ appVersion d) `B.append` " (Bounty)"
apply Quit _ _ = return Nothing
apply s _ _ = return $ Just $ B.pack $ "No action taken: " ++ show s ++ "\r\n"

isQuit :: Command -> Bool
isQuit Quit = True
isQuit _ = False
