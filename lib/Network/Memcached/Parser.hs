{-# LANGUAGE OverloadedStrings #-}
module Network.Memcached.Parser
       ( command
       ) where

import Prelude hiding (take, takeWhile)
import Control.Applicative
import Data.Attoparsec
import Data.Attoparsec.ByteString.Char8 (decimal, stringCI)
import Data.Word
import Network.Memcached.Command
import Network.Memcached.Types

command :: Parser Command
command = choice [ add, cas, get, set, decr, incr, quit, stats, touch
                 , slabsReassign, slabsAutomove, append, delete, prepend
                 , replace, version, flushAll, verbosity ]

set :: Parser Command
set = stringCI "set" >> SetCommand <$> key <*> flags <*> exptime >>= bytesM
      >>= replyM >>= contentM

add :: Parser Command
add = stringCI "add" >> AddCommand <$> key <*> flags <*> exptime >>= bytesM
      >>= replyM >>= contentM

replace :: Parser Command
replace = stringCI "replace" >> ReplaceCommand <$> key <*> flags <*> exptime
          >>= bytesM >>= replyM >>= contentM

append :: Parser Command
append = stringCI "append" >> AppendCommand <$> key <*> flags <*> exptime
         >>= bytesM >>= replyM >>= contentM

prepend :: Parser Command
prepend = stringCI "prepend" >> PrependCommand <$> key <*> flags <*> exptime
          >>= bytesM >>= replyM >>= contentM

cas :: Parser Command
cas = stringCI "cas" >> CasCommand <$> key <*> flags <*> exptime >>= bytesM
      >>= casUniqueM >>= replyM >>= contentM

get :: Parser Command
get = stringCI "get" >> option "" (stringCI "s") >> GetCommand <$> many' key
      <* newline

delete :: Parser Command
delete = stringCI "delete" >> DeleteCommand <$> key <*> reply <* newline

incr :: Parser Command
incr = stringCI "incr" >> IncrementCommand <$> key <*> integer <*> reply
       <* newline

decr :: Parser Command
decr = stringCI "decr" >> DecrementCommand <$> key <*> integer <*> reply
       <* newline

touch :: Parser Command
touch = stringCI "touch" >> TouchCommand <$> key <*> exptime <*> reply
        <* newline

slabsReassign :: Parser Command
slabsReassign = stringCI "slabs" >> skipSpace1 >> stringCI "reassign"
                >> SlabsReassignCommand <$> nInteger <*> integer <* newline

slabsAutomove :: Parser Command
slabsAutomove = stringCI "slabs" >> skipSpace1 >> stringCI "automove"
                >> SlabsAutomoveCommand <$> integer <* newline

stats :: Parser Command
stats = stringCI "stats" >> StatisticsCommand <$> optionMaybe statisticsOption
        <* newline

flushAll :: Parser Command
flushAll = stringCI "flush_all" >> FlushAllCommand <$> optionMaybe integer
           <*> reply <* newline

version :: Parser Command
version = stringCI "version" >> newline >> return VersionCommand

verbosity :: Parser Command
verbosity = stringCI "verbosity" >> VerbosityCommand <$> verbosityLevel
            <* newline

quit :: Parser Command
quit = stringCI "quit" >> newline >> return QuitCommand

--

key :: Parser Key
key = skipSpace1 >> takeWhile1 isPrintable

flags :: Parser Flags
flags = skipSpace1 >> decimal

exptime :: Parser Exptime
exptime = skipSpace1 >> decimal

bytesM :: a -> Parser (a, Bytes)
bytesM a = (,) <$> pure a <*> bytes

bytes :: Parser Bytes
bytes = skipSpace1 >> decimal

replyM :: (Reply -> a, b) -> Parser (a, b)
replyM (a, b) = (,) <$> fmap a reply <*> pure b <* newline

reply :: Parser Reply
reply = option True $ skipSpace1 >> stringCI "noreply" >> return False

contentM :: (Content -> Command, Bytes) -> Parser Command
contentM (a, b) = a <$> take b <* newline

casUniqueM :: (CasUnique -> a, b) -> Parser (a, b)
casUniqueM (a, b) = (,) <$> fmap a casUnique <*> pure b

casUnique :: Parser CasUnique
casUnique = skipSpace1 >> decimal

integer :: Parser Integer
integer = skipSpace1 >> decimal

nInteger :: Parser Integer
nInteger = do
  skipSpace1
  f <- option id $ word8 45 >> return negate
  fmap f decimal

statisticsOption :: Parser StatisticsOption
statisticsOption = skipSpace1 >> choice
                     [ stringCI "settings" >> return StatisticsOptionSettings
                     , stringCI "items" >> return StatisticsOptionItems
                     , stringCI "slabs" >> return StatisticsOptionSlabs
                     , stringCI "sizes" >> return StatisticsOptionSizes
                     ]

verbosityLevel :: Parser VerbosityLevel
verbosityLevel = skipSpace1 >> decimal

newline :: Parser Word8
newline = skipSpace >> word8 13 >> word8 10

skipSpace :: Parser ()
skipSpace = skipWhile isSpace

skipSpace1 :: Parser ()
skipSpace1 = skip isSpace >> skipWhile isSpace

isSpace :: Word8 -> Bool
isSpace w = (w == 32) || (w >= 9 && w <= 12)

isPrintable :: Word8 -> Bool
isPrintable w = (w >= 33) && (w /= 127)

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe = option Nothing . fmap Just
