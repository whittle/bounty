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
  [ testCase "without noreply" _case_set
  , testCase "with noreply" _case_set_noreply
  ]
_case_set = assertCommandParse "set asdf 1 2 3\r\nqwe\r\n"
            $ SetCommand "asdf" 1 2 True "qwe"
_case_set_noreply = assertCommandParse "set asdf 1 2 3 noreply\r\nqwe\r\n"
                    $ SetCommand "asdf" 1 2 False "qwe"

test_add =
  [ testCase "without noreply" _case_add
  , testCase "with noreply" _case_add_noreply
  ]
_case_add = assertCommandParse "add sdfg 4 5 6\r\nzxcvbn\r\n"
            $ AddCommand "sdfg" 4 5 True "zxcvbn"
_case_add_noreply = assertCommandParse "add sdfg 4 5 6 noreply\r\nzxcvbn\r\n"
                    $ AddCommand "sdfg" 4 5 False "zxcvbn"

test_replace =
  [ testCase "without noreply" _case_replace
  , testCase "with noreply" _case_replace_noreply
  ]
_case_replace = assertCommandParse "replace dfgh 7 8 9\r\nqwertyuio\r\n"
                $ ReplaceCommand "dfgh" 7 8 True "qwertyuio"
_case_replace_noreply = assertCommandParse "replace dfgh 7 8 9 noreply\r\nqwertyuio\r\n"
                        $ ReplaceCommand "dfgh" 7 8 False "qwertyuio"

test_append =
  [ testCase "without noreply" _case_append
  , testCase "with noreply" _case_append_noreply
  ]
_case_append = assertCommandParse "append fghj 10 11 12\r\nqwertyuiop[]\r\n"
               $ AppendCommand "fghj" 10 11 True "qwertyuiop[]"
_case_append_noreply = assertCommandParse "append fghj 10 11 12 noreply\r\nqwertyuiop[]\r\n"
                       $ AppendCommand "fghj" 10 11 False "qwertyuiop[]"

test_prepend =
  [ testCase "without noreply" _case_prepend
  , testCase "with noreply" _case_prepend_noreply
  ]
_case_prepend = assertCommandParse "prepend ghjk 13 14 15\r\nqwertyuizxcvbnm\r\n"
                $ PrependCommand "ghjk" 13 14 True "qwertyuizxcvbnm"
_case_prepend_noreply = assertCommandParse "prepend ghjk 13 14 15 noreply\r\nqwertyuizxcvbnm\r\n"
                        $ PrependCommand "ghjk" 13 14 False "qwertyuizxcvbnm"

test_cas =
  [ testCase "without noreply" _case_cas
  , testCase "with noreply" _case_cas_noreply
  ]
_case_cas = assertCommandParse "cas hjkl 16 17 18 19\r\nqwertyuiop[]zxcvbn\r\n"
            $ CasCommand "hjkl" 16 17 19 True "qwertyuiop[]zxcvbn"
_case_cas_noreply = assertCommandParse "cas hjkl 16 17 18 19 noreply\r\nqwertyuiop[]zxcvbn\r\n"
                    $ CasCommand "hjkl" 16 17 19 False "qwertyuiop[]zxcvbn"

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

assertCommandParse b c = parseOnly command b @?= Right c
