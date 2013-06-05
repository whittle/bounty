{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Tests.Command (tests) where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit

import Network.Memcached.Command

tests = $(testGroupGenerator)

test_set =
  [ testCase "without existing key" _case_set
  , testCase "with existing key" _case_set_overwrite
  ]
_case_set = do
  s <- setState Map.empty
  msg <- apply (Set "foo" 0 0 True "bar") settings s
  msg @?= Just "STORED\r\n"
  assertState s $ Map.fromList [("foo", "bar")]
_case_set_overwrite = do
  s <- setState $ Map.fromList [("foo", "bar")]
  msg <- apply (Set "foo" 0 0 True "baz") settings s
  msg @?= Just "STORED\r\n"
  assertState s $ Map.fromList [("foo", "baz")]

test_add =
  [ testCase "without existing key" _case_add
  , testCase "with existing key" _case_add_wont
  ]
_case_add = do
  s <- setState Map.empty
  msg <- apply (Add "foo" 0 0 True "bar") settings s
  msg @?= Just "STORED\r\n"
  assertState s $ Map.fromList [("foo", "bar")]
_case_add_wont = do
  s <- setState $ Map.fromList [("foo", "bar")]
  msg <- apply (Add "foo" 0 0 True "baz") settings s
  msg @?= Just "NOT_STORED\r\n"
  assertState s $ Map.fromList [("foo", "bar")]

test_replace =
  [ testCase "with existing key" _case_replace
  , testCase "without existing key" _case_replace_wont
  ]
_case_replace = do
  s <- setState $ Map.fromList [("foo", "bar")]
  msg <- apply (Replace "foo" 0 0 True "baz") settings s
  msg @?= Just "STORED\r\n"
  assertState s $ Map.fromList [("foo", "baz")]
_case_replace_wont = do
  s <- setState Map.empty
  msg <- apply (Replace "foo" 0 0 True "bar") settings s
  msg @?= Just "NOT_STORED\r\n"
  assertState s Map.empty

test_append =
  [ testCase "with existing key" _case_append
  , testCase "without existing key" _case_append_wont
  ]
_case_append = do
  s <- setState $ Map.fromList [("foo", "bar")]
  msg <- apply (Append "foo" 0 0 True "baz") settings s
  msg @?= Just "STORED\r\n"
  assertState s $ Map.fromList [("foo", "barbaz")]
_case_append_wont = do
  s <- setState Map.empty
  msg <- apply (Append "foo" 0 0 True "bar") settings s
  msg @?= Just "NOT_STORED\r\n"
  assertState s Map.empty

test_prepend =
  [ testCase "with existing key" _case_prepend
  , testCase "without existing key" _case_prepend_wont
  ]
_case_prepend = do
  s <- setState $ Map.fromList [("foo", "bar")]
  msg <- apply (Prepend "foo" 0 0 True "baz") settings s
  msg @?= Just "STORED\r\n"
  assertState s $ Map.fromList [("foo", "bazbar")]
_case_prepend_wont = do
  s <- setState Map.empty
  msg <- apply (Prepend "foo" 0 0 True "bar") settings s
  msg @?= Just "NOT_STORED\r\n"
  assertState s Map.empty

setState = atomically . newTVar
settings = undefined
assertState s m = readTVarIO s >>= (m@?=)
