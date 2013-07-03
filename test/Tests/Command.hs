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

setState = atomically . newTVar . Map.fromList
settings = undefined
assertState s m = readTVarIO s >>= (@?=) (Map.fromList m)
