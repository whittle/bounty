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
import Network.Memcached.Types

tests = $(testGroupGenerator)

test_set =
  [ testCase "without existing key" $ do
       s <- setState []
       msg <- apply (Set "foo" 0 0 True "bar") settings s
       msg @?= Just "STORED\r\n"
       assertState s [("foo", Record 0 Nothing 0 "bar")]
  , testCase "with existing key" $ do
       s <- setState [("foo", Record 1 Nothing 0 "bar")]
       msg <- apply (Set "foo" 1 0 True "baz") settings s
       msg @?= Just "STORED\r\n"
       assertState s [("foo", Record 1 Nothing 0 "baz")]
  , testCase "noreply" $ do
       s <- setState []
       msg <- apply (Set "foo" 1 0 False "bar") settings s
       msg @?= Nothing
       assertState s [("foo", Record 1 Nothing 0 "bar")]
  ]

test_add =
  [ testCase "without existing key" $ do
       s <- setState []
       msg <- apply (Add "foo" 2 0 True "bar") settings s
       msg @?= Just "STORED\r\n"
       assertState s [("foo", Record 2 Nothing 0 "bar")]
  , testCase "with existing key" $ do
       s <- setState [("foo", Record 3 Nothing 0 "bar")]
       msg <- apply (Add "foo" 5 0 True "baz") settings s
       msg @?= Just "NOT_STORED\r\n"
       assertState s [("foo", Record 3 Nothing 0 "bar")]
  , testCase "noreply" $ do
       s <- setState []
       msg <- apply (Add "foo" 8 0 False "bar") settings s
       msg @?= Nothing
       assertState s [("foo", Record 8 Nothing 0 "bar")]
  ]

test_replace =
  [ testCase "with existing key" $ do
       s <- setState [("foo", Record 13 Nothing 0 "bar")]
       msg <- apply (Replace "foo" 21 0 True "baz") settings s
       msg @?= Just "STORED\r\n"
       assertState s [("foo", Record 21 Nothing 0 "baz")]
  , testCase "without existing key" $ do
       s <- setState []
       msg <- apply (Replace "foo" 34 0 True "bar") settings s
       msg @?= Just "NOT_STORED\r\n"
       assertState s []
  , testCase "noreply" $ do
       s <- setState []
       msg <- apply (Replace "foo" 55 0 False "bar") settings s
       msg @?= Nothing
       assertState s []
  ]

test_append =
  [ testCase "with existing key" $ do
       s <- setState [("foo", Record 89 Nothing 0 "bar")]
       msg <- apply (Append "foo" 144 0 True "baz") settings s
       msg @?= Just "STORED\r\n"
       assertState s [("foo", Record 144 Nothing 0 "barbaz")]
  , testCase "without existing key" $ do
       s <- setState []
       msg <- apply (Append "foo" 233 0 True "bar") settings s
       msg @?= Just "NOT_STORED\r\n"
       assertState s []
  , testCase "noreply" $ do
       s <- setState []
       msg <- apply (Append "foo" 377 0 False "bar") settings s
       msg @?= Nothing
       assertState s []
  ]

test_prepend =
  [ testCase "with existing key" $ do
       s <- setState [("foo", Record 610 Nothing 0 "bar")]
       msg <- apply (Prepend "foo" 987 0 True "baz") settings s
       msg @?= Just "STORED\r\n"
       assertState s [("foo", Record 987 Nothing 0 "bazbar")]
  , testCase "without existing key" $ do
       s <- setState []
       msg <- apply (Prepend "foo" 1597 0 True "bar") settings s
       msg @?= Just "NOT_STORED\r\n"
       assertState s []
  , testCase "noreply" $ do
       s <- setState []
       msg <- apply (Prepend "foo" 2584 0 False "bar") settings s
       msg @?= Nothing
       assertState s []
  ]

test_get =
  [ testCase "with no keys" $ do
       s <- setState []
       msg <- apply (Get []) settings s
       msg @?= Just "END\r\n"
  , testCase "with one existing key" $ do
       s <- setState [("foo", Record 4181 Nothing 0 "bar")]
       msg <- apply (Get ["foo"]) settings s
       msg @?= Just "VALUE foo 4181 3\r\nbar\r\nEND\r\n"
  , testCase "with one missing key" $ do
       s <- setState []
       msg <- apply (Get ["foo"]) settings s
       msg @?= Just "END\r\n"
  , testCase "with mixed existing and missing keys" $ do
       s <- setState [("foo", Record 6765 Nothing 0 "bar"), ("baz", Record 10946 Nothing 0 "quux")]
       msg <- apply (Get ["foo", "nil", "baz", "null"]) settings s
       msg @?= Just "VALUE foo 6765 3\r\nbar\r\nVALUE baz 10946 4\r\nquux\r\nEND\r\n"
  ]

test_delete =
  [ testCase "with an existing key" $ do
       s <- setState [("foo", Record 17711 Nothing 0 "bar"), ("baz", Record 28657 Nothing 0 "quux")]
       msg <- apply (Delete "foo" True) settings s
       msg @?= Just "DELETED\r\n"
       assertState s [("baz", Record 28657 Nothing 0 "quux")]
  , testCase "with a missing key" $ do
       s <- setState [("baz", Record 46368 Nothing 0 "quux")]
       msg <- apply (Delete "foo" True) settings s
       msg @?= Just "NOT_FOUND\r\n"
       assertState s [("baz", Record 46368 Nothing 0 "quux")]
  , testCase "with no reply" $ do
       s <- setState [("foo", Record 75025 Nothing 0 "bar"), ("baz", Record 121393 Nothing 0 "quux")]
       msg <- apply (Delete "foo" False) settings s
       msg @?= Nothing
       assertState s [("baz", Record 121393 Nothing 0 "quux")]
  ]

test_increment =
  [ testCase "with an existing key" $ do
       s <- setState [("foo", Record 196418 Nothing 0 "23")]
       msg <- apply (Increment "foo" 19 True) settings s
       msg @?= Just "42\r\n"
       assertState s [("foo", Record 196418 Nothing 0 "42")]
  , testCase "with a missing key" $ do
       s <- setState []
       msg <- apply (Increment "foo" 19 True) settings s
       msg @?= Just "NOT_FOUND\r\n"
       assertState s []
  , testCase "with no reply" $ do
       s <- setState [("foo", Record 317811 Nothing 0 "23")]
       msg <- apply (Increment "foo" 19 False) settings s
       msg @?= Nothing
       assertState s [("foo", Record 317811 Nothing 0 "42")]
  ]

test_decrement =
  [ testCase "with an existing key" $ do
       s <- setState [("foo", Record 514229 Nothing 0 "61")]
       msg <- apply (Decrement "foo" 19 True) settings s
       msg @?= Just "42\r\n"
       assertState s [("foo", Record 514229 Nothing 0 "42")]
  , testCase "with a missing key" $ do
       s <- setState []
       msg <- apply (Decrement "foo" 19 True) settings s
       msg @?= Just "NOT_FOUND\r\n"
       assertState s []
  , testCase "with no reply" $ do
       s <- setState [("foo", Record 832040 Nothing 0 "61")]
       msg <- apply (Decrement "foo" 19 False) settings s
       msg @?= Nothing
       assertState s [("foo", Record 832040 Nothing 0 "42")]
  ]

test_touch =
  [ testCase "with an existing key" $ do
       s <- setState [("foo", Record 0 Nothing 0 "bar")]
       msg <- apply (Touch "foo" 0 True) settings s
       msg @?= Just "TOUCHED\r\n"
       assertState s [("foo", Record 0 Nothing 0 "bar")]
  , testCase "with a missing key" $ do
       s <- setState []
       msg <- apply (Touch "foo" 0 True) settings s
       msg @?= Just "NOT_FOUND\r\n"
       assertState s []
  , testCase "with no reply" $ do
       s <- setState []
       msg <- apply (Touch "foo" 0 False) settings s
       msg @?= Nothing
       assertState s []
  ]

test_flush_all =
  [ testCase "with no argument" $ do
       s <- setState [("foo", Record 0 Nothing 0 "bar"), ("baz", Record 0 Nothing 0 "quux")]
       msg <- apply (FlushAll Nothing True) settings s
       msg @?= Just "OK\r\n"
       assertState s []
  , testCase "noreply" $ do
       s <- setState [("foo", Record 0 Nothing 0 "bar"), ("baz", Record 0 Nothing 0 "quux")]
       msg <- apply (FlushAll Nothing False) settings s
       msg @?= Nothing
       assertState s []
  ]

test_verbosity =
  [ testCase "with reply" $ do
       msg <- apply (Verbosity 0 True) settings undefined
       msg @?= Just "OK\r\n"
  , testCase "with no reply" $ do
       msg <- apply (Verbosity 0 False) settings undefined
       msg @?= Nothing
  ]

setState = atomically . newTVar . Map.fromList
assertState s m = readTVarIO s >>= (@?=) (Map.fromList m)

settings = undefined
