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
set = stringCI "set" >> Set <$> key <*> flags <*> exptime >>= bytesM
      >>= replyM >>= contentM

add :: Parser Command
add = stringCI "add" >> Add <$> key <*> flags <*> exptime >>= bytesM
      >>= replyM >>= contentM

replace :: Parser Command
replace = stringCI "replace" >> Replace <$> key <*> flags <*> exptime
          >>= bytesM >>= replyM >>= contentM

append :: Parser Command
append = stringCI "append" >> Append <$> key <*> flags <*> exptime
         >>= bytesM >>= replyM >>= contentM

prepend :: Parser Command
prepend = stringCI "prepend" >> Prepend <$> key <*> flags <*> exptime
          >>= bytesM >>= replyM >>= contentM

cas :: Parser Command
cas = stringCI "cas" >> Cas <$> key <*> flags <*> exptime >>= bytesM
      >>= casUniqueM >>= replyM >>= contentM

get :: Parser Command
get = stringCI "get" >> option "" (stringCI "s") >> Get <$> many' key
      <* newline

delete :: Parser Command
delete = stringCI "delete" >> Delete <$> key <*> reply <* newline

incr :: Parser Command
incr = stringCI "incr" >> Increment <$> key <*> integer <*> reply
       <* newline

decr :: Parser Command
decr = stringCI "decr" >> Decrement <$> key <*> integer <*> reply
       <* newline

touch :: Parser Command
touch = stringCI "touch" >> Touch <$> key <*> exptime <*> reply
        <* newline

slabsReassign :: Parser Command
slabsReassign = stringCI "slabs" >> space >> stringCI "reassign"
                >> SlabsReassign <$> nInteger <*> integer <* newline

slabsAutomove :: Parser Command
slabsAutomove = stringCI "slabs" >> space >> stringCI "automove"
                >> SlabsAutomove <$> integer <* newline

stats :: Parser Command
stats = stringCI "stats" >> Statistics <$> optionMaybe statisticsOption
        <* newline

flushAll :: Parser Command
flushAll = stringCI "flush_all" >> FlushAll <$> optionMaybe integer
           <*> reply <* newline

version :: Parser Command
version = stringCI "version" >> newline >> return Version

verbosity :: Parser Command
verbosity = stringCI "verbosity" >> Verbosity <$> verbosityLevel
            <* newline

quit :: Parser Command
quit = stringCI "quit" >> newline >> return Quit

--

key :: Parser Key
key = space >> takeWhile1 isPrintable

flags :: Parser Flags
flags = space >> decimal

exptime :: Parser Exptime
exptime = space >> decimal

bytesM :: a -> Parser (a, Bytes)
bytesM a = (,) <$> pure a <*> bytes

bytes :: Parser Bytes
bytes = space >> decimal

replyM :: (Reply -> a, b) -> Parser (a, b)
replyM (a, b) = (,) <$> fmap a reply <*> pure b <* newline

reply :: Parser Reply
reply = option True $ space >> stringCI "noreply" >> return False

contentM :: (Content -> Command, Bytes) -> Parser Command
contentM (a, b) = a <$> take b <* newline

casUniqueM :: (CasUnique -> a, b) -> Parser (a, b)
casUniqueM (a, b) = (,) <$> fmap a casUnique <*> pure b

casUnique :: Parser CasUnique
casUnique = space >> decimal

integer :: Parser Integer
integer = space >> decimal

nInteger :: Parser Integer
nInteger = space >> option id (word8 45 >> return negate) >>= flip fmap decimal

statisticsOption :: Parser StatisticsOption
statisticsOption = space >> choice
                     [ stringCI "settings" >> return StatisticsOptionSettings
                     , stringCI "items" >> return StatisticsOptionItems
                     , stringCI "slabs" >> return StatisticsOptionSlabs
                     , stringCI "sizes" >> return StatisticsOptionSizes
                     ]

verbosityLevel :: Parser VerbosityLevel
verbosityLevel = space >> decimal

newline :: Parser Word8
newline = skipWhile isSpace >> word8 13 >> word8 10

space :: Parser ()
space = skip isSpace >> skipWhile isSpace

isSpace :: Word8 -> Bool
isSpace w = (w == 32) || (w >= 9 && w <= 12)

isPrintable :: Word8 -> Bool
isPrintable w = (w >= 33) && (w /= 127)

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe = option Nothing . fmap Just
