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

case_set = assertCommandParse "set asdf 1 2 3\r\n"
           $ SetCommand "asdf" 1 2 3 True
case_add = assertCommandParse "add sdfg 4 5 6 noreply\r\n"
           $ AddCommand "sdfg" 4 5 6 False
case_replace = assertCommandParse "replace dfgh 7 8 9\r\n"
               $ ReplaceCommand "dfgh" 7 8 9 True
case_append = assertCommandParse "append fghj 10 11 12 noreply\r\n"
              $ AppendCommand "fghj" 10 11 12 False
case_prepend = assertCommandParse "prepend ghjk 13 14 15\r\n"
               $ PrependCommand "ghjk" 13 14 15 True
case_cas = assertCommandParse "cas hjkl 16 17 18 19 noreply\r\n"
           $ CasCommand "hjkl" 16 17 18 19 False

test_delete =
  [ testCase "without a time" _case_delete
  , testCase "with a time" _case_delete_time
  ]
_case_delete = assertCommandParse "delete jkl;\r\n"
               $ DeleteCommand "jkl;" Nothing True
_case_delete_time = assertCommandParse "delete qwer 20 noreply\r\n"
                    $ DeleteCommand "qwer" (Just 20) False

case_incr = assertCommandParse "incr wert 21\r\n"
            $ IncrementCommand "wert" 21 True
case_decr = assertCommandParse "decr erty 22 noreply\r\n"
            $ DecrementCommand "erty" 22 False

test_flushAll =
  [ testCase "without an int" _case_flushAll
  , testCase "with an int" _case_flushAll_int
  ]
_case_flushAll = assertCommandParse "flush_all\r\n"
                 $ FlushAllCommand Nothing True
_case_flushAll_int = assertCommandParse "flush_all 23 noreply\r\n"
                     $ FlushAllCommand (Just 23) False

case_get = assertCommandParse "get rtyu\r\n" $ GetCommand ["rtyu"]
case_gets = assertCommandParse "gets tyui yuio uiop\r\n"
            $ GetsCommand ["tyui", "yuio", "uiop"]

test_stats =
  [ testCase "with no argument" _case_stats
  , testCase "with items argument" _case_stats_items
  , testCase "with slabs argument" _case_stats_slabs
  , testCase "with sizes argument" _case_stats_sizes
  ]
_case_stats = assertCommandParse "stats\r\n"
              $ StatisticsCommand Nothing
_case_stats_items = assertCommandParse "stats items\r\n"
                    $ StatisticsCommand $ Just StatisticsOptionItems
_case_stats_slabs = assertCommandParse "stats slabs\r\n"
                    $ StatisticsCommand $ Just StatisticsOptionSlabs
_case_stats_sizes = assertCommandParse "stats sizes\r\n"
                    $ StatisticsCommand $ Just StatisticsOptionSizes

case_version = assertCommandParse "version\r\n" VersionCommand
case_verbosity = assertCommandParse "verbosity 0\r\n" $ VerbosityCommand 0
case_quit = assertCommandParse "quit\r\n" QuitCommand

assertCommandParse b c = parseOnly commands b @?= Right [c]
