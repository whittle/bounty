module Network.Memcached.Token where

data Token = TokenSet
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

           | TokenKey String
           | TokenInteger Integer
           | TokenNoReply
           | TokenItems
           | TokenSlabs
           | TokenSizes
           deriving Show
