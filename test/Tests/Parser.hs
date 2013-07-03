{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Parser (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit

import Data.Attoparsec

import Network.Memcached.Command
import Network.Memcached.Parser
import Network.Memcached.Types

tests = $(testGroupGenerator)

test_set =
  [ testCase "without noreply"
    $ "set asdf 1 2 3\r\nqwe\r\n" @<?= Set "asdf" 1 2 True "qwe"
  , testCase "with noreply"
    $ "set asdf 1 2 3 noreply\r\nqwe\r\n" @<?= Set "asdf" 1 2 False "qwe"
  ]

test_add =
  [ testCase "without noreply"
    $ "add sdfg 4 5 6\r\nzxcvbn\r\n" @<?= Add "sdfg" 4 5 True "zxcvbn"
  , testCase "with noreply"
    $ "add sdfg 4 5 6 noreply\r\nzxcvbn\r\n" @<?= Add "sdfg" 4 5 False "zxcvbn"
  ]

test_replace =
  [ testCase "without noreply"
    $ "replace dfgh 7 8 9\r\nqwertyuio\r\n" @<?= Replace "dfgh" 7 8 True "qwertyuio"
  , testCase "with noreply"
    $ "replace dfgh 7 8 9 noreply\r\nqwertyuio\r\n" @<?= Replace "dfgh" 7 8 False "qwertyuio"
  ]

test_append =
  [ testCase "without noreply"
    $ "append fghj 10 11 12\r\nqwertyuiop[]\r\n" @<?= Append "fghj" 10 11 True "qwertyuiop[]"
  , testCase "with noreply"
    $ "append fghj 10 11 12 noreply\r\nqwertyuiop[]\r\n" @<?= Append "fghj" 10 11 False "qwertyuiop[]"
  ]

test_prepend =
  [ testCase "without noreply"
    $ "prepend ghjk 13 14 15\r\nqwertyuizxcvbnm\r\n" @<?= Prepend "ghjk" 13 14 True "qwertyuizxcvbnm"
  , testCase "with noreply"
    $ "prepend ghjk 13 14 15 noreply\r\nqwertyuizxcvbnm\r\n" @<?= Prepend "ghjk" 13 14 False "qwertyuizxcvbnm"
  ]

test_cas =
  [ testCase "without noreply"
    $ "cas hjkl 16 17 18 19\r\nqwertyuiop[]zxcvbn\r\n" @<?= Cas "hjkl" 16 17 19 True "qwertyuiop[]zxcvbn"
  , testCase "with noreply"
    $ "cas hjkl 16 17 18 19 noreply\r\nqwertyuiop[]zxcvbn\r\n" @<?= Cas "hjkl" 16 17 19 False "qwertyuiop[]zxcvbn"
  ]

case_get = "get rtyu\r\n" @<?= Get ["rtyu"]
case_gets = "gets tyui yuio uiop\r\n" @<?= Get ["tyui", "yuio", "uiop"]
case_delete = "delete jkl;\r\n" @<?= Delete "jkl;" True
case_incr = "incr wert 21\r\n" @<?= Increment "wert" 21 True
case_decr = "decr erty 22 noreply\r\n" @<?= Decrement "erty" 22 False
case_touch = "touch rtyu 23\r\n" @<?= Touch "rtyu" 23 True
case_slabs_reassign = "slabs reassign -1 24\r\n" @<?= SlabsReassign (-1) 24
case_slabs_automove = "slabs automove 0\r\n" @<?= SlabsAutomove 0

test_stats =
  [ testCase "with no argument" $ "stats\r\n" @<?= Statistics Nothing
  , testCase "with settings argument" $ "stats settings\r\n" @<?= (Statistics $ Just StatSettings)
  , testCase "with items argument" $ "stats items\r\n" @<?= (Statistics $ Just StatItems)
  , testCase "with slabs argument" $ "stats slabs\r\n" @<?= (Statistics $ Just StatSlabs)
  , testCase "with sizes argument" $ "stats sizes\r\n" @<?= (Statistics $ Just StatSizes)
  ]

test_flushAll =
  [ testCase "without an int" $ "flush_all\r\n" @<?= FlushAll Nothing True
  , testCase "with an int" $ "flush_all 23 noreply\r\n" @<?= FlushAll (Just 23) False
  ]

case_version = assertCommandParse "version\r\n" Version
case_verbosity = assertCommandParse "verbosity 0\r\n" $ Verbosity 0
case_quit = assertCommandParse "quit\r\n" Quit

assertCommandParse b c = parseOnly command b @?= Right c
(@<?=) = assertCommandParse
