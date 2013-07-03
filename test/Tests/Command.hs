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
  [ testCase "without existing key" $ do
       s <- setState []
       msg <- apply (Set "foo" 0 0 True "bar") settings s
       msg @?= Just "STORED\r\n"
       assertState s [("foo", "bar")]
  , testCase "with existing key" $ do
       s <- setState [("foo", "bar")]
       msg <- apply (Set "foo" 0 0 True "baz") settings s
       msg @?= Just "STORED\r\n"
       assertState s [("foo", "baz")]
  , testCase "noreply" $ do
       s <- setState []
       msg <- apply (Set "foo" 0 0 False "bar") settings s
       msg @?= Nothing
  ]

test_add =
  [ testCase "without existing key" $ do
       s <- setState []
       msg <- apply (Add "foo" 0 0 True "bar") settings s
       msg @?= Just "STORED\r\n"
       assertState s [("foo", "bar")]
  , testCase "with existing key" $ do
       s <- setState [("foo", "bar")]
       msg <- apply (Add "foo" 0 0 True "baz") settings s
       msg @?= Just "NOT_STORED\r\n"
       assertState s [("foo", "bar")]
  , testCase "noreply" $ do
       s <- setState []
       msg <- apply (Add "foo" 0 0 False "bar") settings s
       msg @?= Nothing
  ]

test_replace =
  [ testCase "with existing key" $ do
       s <- setState [("foo", "bar")]
       msg <- apply (Replace "foo" 0 0 True "baz") settings s
       msg @?= Just "STORED\r\n"
       assertState s [("foo", "baz")]
  , testCase "without existing key" $ do
       s <- setState []
       msg <- apply (Replace "foo" 0 0 True "bar") settings s
       msg @?= Just "NOT_STORED\r\n"
       assertState s []
  , testCase "noreply" $ do
       s <- setState []
       msg <- apply (Replace "foo" 0 0 False "bar") settings s
       msg @?= Nothing
  ]

test_append =
  [ testCase "with existing key" $ do
       s <- setState [("foo", "bar")]
       msg <- apply (Append "foo" 0 0 True "baz") settings s
       msg @?= Just "STORED\r\n"
       assertState s [("foo", "barbaz")]
  , testCase "without existing key" $ do
       s <- setState []
       msg <- apply (Append "foo" 0 0 True "bar") settings s
       msg @?= Just "NOT_STORED\r\n"
       assertState s []
  , testCase "noreply" $ do
       s <- setState []
       msg <- apply (Append "foo" 0 0 False "bar") settings s
       msg @?= Nothing
  ]

test_prepend =
  [ testCase "with existing key" $ do
       s <- setState [("foo", "bar")]
       msg <- apply (Prepend "foo" 0 0 True "baz") settings s
       msg @?= Just "STORED\r\n"
       assertState s [("foo", "bazbar")]
  , testCase "without existing key" $ do
       s <- setState []
       msg <- apply (Prepend "foo" 0 0 True "bar") settings s
       msg @?= Just "NOT_STORED\r\n"
       assertState s []
  , testCase "noreply" $ do
       s <- setState []
       msg <- apply (Prepend "foo" 0 0 False "bar") settings s
       msg @?= Nothing
  ]

test_get =
  [ testCase "with no keys" $ do
       s <- setState []
       msg <- apply (Get []) settings s
       msg @?= Just "END\r\n"
  , testCase "with one existing key" $ do
       s <- setState [("foo", "bar")]
       msg <- apply (Get ["foo"]) settings s
       msg @?= Just "VALUE foo 0 3\r\nbar\r\nEND\r\n"
  , testCase "with one missing key" $ do
       s <- setState []
       msg <- apply (Get ["foo"]) settings s
       msg @?= Just "END\r\n"
  , testCase "with mixed existing and missing keys" $ do
       s <- setState [("foo", "bar"), ("baz", "quux")]
       msg <- apply (Get ["foo", "nil", "baz", "null"]) settings s
       msg @?= Just "VALUE foo 0 3\r\nbar\r\nVALUE baz 0 4\r\nquux\r\nEND\r\n"
  ]

test_delete =
  [ testCase "for an existing key" $ do
       s <- setState [("foo", "bar"), ("baz", "quux")]
       msg <- apply (Delete "foo" True) settings s
       msg @?= Just "DELETED\r\n"
       assertState s [("baz", "quux")]
  , testCase "for a missing key" $ do
       s <- setState [("baz", "quux")]
       msg <- apply (Delete "foo" True) settings s
       msg @?= Just "NOT_FOUND\r\n"
       assertState s [("baz", "quux")]
  , testCase "noreply" $ do
       s <- setState [("foo", "bar"), ("baz", "quux")]
       msg <- apply (Delete "foo" False) settings s
       msg @?= Nothing
  ]

test_flush_all =
  [ testCase "with no argument" $ do
       s <- setState [("foo", "bar"), ("baz", "quux")]
       msg <- apply (FlushAll Nothing True) settings s
       msg @?= Just "OK\r\n"
       assertState s []
  , testCase "noreply" $ do
       s <- setState [("foo", "bar"), ("baz", "quux")]
       msg <- apply (FlushAll Nothing False) settings s
       msg @?= Nothing
  ]

setState = atomically . newTVar . Map.fromList
settings = undefined
assertState s m = readTVarIO s >>= (@?=) (Map.fromList m)
