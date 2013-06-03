{-# LANGUAGE OverloadedStrings #-}
module Network.Memcached.Parser
       ( command
       ) where

import Prelude hiding (take, takeWhile)
import Control.Applicative
import Data.Attoparsec hiding (string)
import Data.Attoparsec.ByteString.Char8 (decimal, stringCI)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Network.Memcached.Command (Command(..))
import Network.Memcached.Types

string :: ByteString -> Parser ByteString
string = stringCI

command :: Parser Command
command = choice [ add, cas, get, set, decr, incr, quit, stats, touch
                 , slabsReassign, slabsAutomove, append, delete, prepend
                 , replace, version, flushAll, verbosity ]

set :: Parser Command
set = string "set" >> Set <$> key <*> flags <*> exptime >>= bytesM >>= replyM
      >>= contentM

add :: Parser Command
add = string "add" >> Add <$> key <*> flags <*> exptime >>= bytesM >>= replyM
      >>= contentM

replace :: Parser Command
replace = string "replace" >> Replace <$> key <*> flags <*> exptime >>= bytesM
          >>= replyM >>= contentM

append :: Parser Command
append = string "append" >> Append <$> key <*> flags <*> exptime >>= bytesM
         >>= replyM >>= contentM

prepend :: Parser Command
prepend = string "prepend" >> Prepend <$> key <*> flags <*> exptime >>= bytesM
          >>= replyM >>= contentM

cas :: Parser Command
cas = string "cas" >> Cas <$> key <*> flags <*> exptime >>= bytesM
      >>= casUniqueM >>= replyM >>= contentM

get :: Parser Command
get = string "get" >> option "" (string "s") >> Get <$> many' key <* newline

delete :: Parser Command
delete = string "delete" >> Delete <$> key <*> reply <* newline

incr :: Parser Command
incr = string "incr" >> Increment <$> key <*> integer <*> reply <* newline

decr :: Parser Command
decr = string "decr" >> Decrement <$> key <*> integer <*> reply <* newline

touch :: Parser Command
touch = string "touch" >> Touch <$> key <*> exptime <*> reply <* newline

slabsReassign :: Parser Command
slabsReassign = string "slabs reassign" >> SlabsReassign <$> nInteger
                <*> integer <* newline

slabsAutomove :: Parser Command
slabsAutomove = string "slabs automove" >> SlabsAutomove <$> integer <* newline

stats :: Parser Command
stats = string "stats" >> Statistics <$> optionMaybe statisticsOption <* newline

flushAll :: Parser Command
flushAll = string "flush_all" >> FlushAll <$> optionMaybe integer <*> reply
           <* newline

version :: Parser Command
version = string "version" >> newline >> return Version

verbosity :: Parser Command
verbosity = string "verbosity" >> Verbosity <$> verbosityLevel <* newline

quit :: Parser Command
quit = string "quit" >> newline >> return Quit

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
reply = option True $ space >> string "noreply" >> return False

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
statisticsOption = space >> choice [ string "settings" >> return StatSettings
                                   , string "items" >> return StatItems
                                   , string "slabs" >> return StatSlabs
                                   , string "sizes" >> return StatSizes
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
