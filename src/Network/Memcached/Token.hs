module Network.Memcached.Token
       ( Token(..)
       ) where

data Token = TokenKey String
           | TokenInteger Integer
             -- Commands
           | TokenSet
           | TokenAdd
           | TokenReplace
           | TokenAppend
           | TokenPrepend
           | TokenCas
           | TokenGet
           | TokenGets
           | TokenDelete
           | TokenIncr
           | TokenDecr
           | TokenStats
           | TokenFlushAll
           | TokenVersion
           | TokenVerbosity
           | TokenQuit
             -- Non-command literals
           | TokenNoReply
           | TokenItems
           | TokenSlabs
           | TokenSizes
           deriving Show
