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
             | Verbosity VerbosityLevel Reply
             | Quit
             deriving (Eq, Show)

-- N.B. This module has gotten ridiculously ugly. Time to refactor.

apply :: Command -> AppData -> State -> IO (Maybe B.ByteString)

apply (Set k f _ r c) _ tm = do
  atomically $ do
    m <- readTVar tm
    let record = Record f Nothing 0 c
    writeTVar tm $ Map.insert k record m
  return $ if r then Just "STORED\r\n" else Nothing

apply (Add k f _ r c) _ tm = do
  msg <- atomically $ do
    m <- readTVar tm
    if Map.member k m
      then return "NOT_STORED\r\n"
      else do let record = Record f Nothing 0 c
              writeTVar tm $ Map.insert k record m
              return "STORED\r\n"
  return $ if r then Just msg else Nothing

apply (Replace k f _ r c) _ tm = do
  msg <- atomically $ do
    m <- readTVar tm
    if Map.notMember k m
      then return "NOT_STORED\r\n"
      else do let record = Record f Nothing 0 c
              writeTVar tm $ Map.insert k record m
              return "STORED\r\n"
  return $ if r then Just msg else Nothing

apply (Append k f _ r c) _ tm = do
  msg <- atomically $ do
    m <- readTVar tm
    if Map.notMember k m
      then return "NOT_STORED\r\n"
      else do let record = m ! k
              let record' = Record f Nothing 0 $ content record <> c
              writeTVar tm $ Map.insert k record' m
              -- TODO: replace the above mess with a lens and insertWith
              return "STORED\r\n"
  return $ if r then Just msg else Nothing

apply (Prepend k f _ r c) _ tm = do
  msg <- atomically $ do
    m <- readTVar tm
    if Map.notMember k m
      then return "NOT_STORED\r\n"
      else do let record = m ! k
              let record' = Record f Nothing 0 $ c <> content record
              writeTVar tm $ Map.insert k record' m
              -- TODO: replace the above mess with a lens and insertWith
              return "STORED\r\n"
  return $ if r then Just msg else Nothing

apply (Get ks) _ tm = do
  m <- readTVarIO tm
  let f = B.pack . show . flags
  let l = B.pack . show . B.length . content
  let print'' k r = B.intercalate " " ["VALUE", k, f r, l r] <> "\r\n" <> content r <> "\r\n"
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

apply (Increment k i r) _ tm = do
  msg <- atomically $ do
    m <- readTVar tm
    if Map.notMember k m
      then return "NOT_FOUND\r\n"
      else do let Record f e c v = m ! k
              let v' = B.pack $ show $ (+i) $ read $ B.unpack v
              writeTVar tm $ Map.insert k (Record f e c v') m
              -- TODO: replace the above mess with a lens and updateLookupWithKey
              return $ v' <> "\r\n"
  return $ if r then Just msg else Nothing

apply (Decrement k i r) _ tm = do
  msg <- atomically $ do
    m <- readTVar tm
    if Map.notMember k m
      then return "NOT_FOUND\r\n"
      else do let Record f e c v = m ! k
              let v' = B.pack $ show $ (subtract i) $ read $ B.unpack v
              writeTVar tm $ Map.insert k (Record f e c v') m
              -- TODO: replace the above mess with a lens and updateLookupWithKey
              return $ v' <> "\r\n"
  return $ if r then Just msg else Nothing

apply (Touch k _ r) _ tm = do
  -- Doesn’t do anything; implement automatic expiration.
  msg <- atomically $ do
    m <- readTVar tm
    if Map.notMember k m
      then return "NOT_FOUND\r\n"
      else return "TOUCHED\r\n"
  return $ if r then Just msg else Nothing

apply (FlushAll Nothing r) _ tm = do
  atomically $ writeTVar tm Map.empty
  return $ if r then Just "OK\r\n" else Nothing

apply Version d _ = return $ Just $ "VERSION " <> (B.pack $ showVersion $ appVersion d) <> " (Bounty)"

apply (Verbosity _ r) _ _ = do
  -- Doesn’t do anything; implement logging?
  return $ if r then Just "OK\r\n" else Nothing

apply Quit _ _ = return Nothing

apply s _ _ = return $ Just $ B.pack $ "No action taken: " ++ show s ++ "\r\n"

isQuit :: Command -> Bool
isQuit Quit = True
isQuit _ = False
