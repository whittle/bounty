{-# LANGUAGE OverloadedStrings #-}
module Network.Memcached.Parser
       ( command
       ) where

import Prelude hiding (takeWhile)
import Control.Applicative
import Data.Attoparsec
import Data.Attoparsec.ByteString.Char8 (decimal, stringCI)
import Data.Word
import Network.Memcached.Command
import Network.Memcached.Types

command :: Parser Command
command = choice [ add, cas, get, set, decr, gets, incr, quit, stats, append
                 , delete, prepend, replace, version, flushAll, verbosity ]

set :: Parser Command
set = stringCI "set" >> SetCommand <$> key <*> flags <*> exptime <*> bytes <*> reply <* newline

add :: Parser Command
add = startAdd >>= bytesAdd >>= replyAdd >>= completeAdd

startAdd :: Parser (Reply -> Content -> Command)
startAdd = stringCI "add" >> AddCommand <$> key <*> flags <*> exptime

bytesAdd :: (Reply -> Content -> Command) -> Parser (Reply -> Content -> Command, Int)
bytesAdd a = (,) <$> return a <*> int

replyAdd :: (Reply -> Content -> Command, Int) -> Parser (Content -> Command, Int)
replyAdd (a, b) = (,) <$> fmap a reply <*> return b <* newline

completeAdd :: (Content -> Command, Int) -> Parser Command
completeAdd (a, b) = a <$> Data.Attoparsec.take b <* newline

replace :: Parser Command
replace = stringCI "replace" >> ReplaceCommand <$> key <*> flags <*> exptime <*> bytes <*> reply <* newline

append :: Parser Command
append = stringCI "append" >> AppendCommand <$> key <*> flags <*> exptime <*> bytes <*> reply <* newline

prepend :: Parser Command
prepend = stringCI "prepend" >> PrependCommand <$> key <*> flags <*> exptime <*> bytes <*> reply <* newline

cas :: Parser Command
cas = stringCI "cas" >> CasCommand <$> key <*> flags <*> exptime <*> bytes <*> casUnique <*> reply <* newline

delete :: Parser Command
delete = stringCI "delete" >> DeleteCommand <$> key <*> optionMaybe time <*> reply <* newline

incr :: Parser Command
incr = stringCI "incr" >> IncrementCommand <$> key <*> integer <*> reply <* newline

decr :: Parser Command
decr = stringCI "decr" >> DecrementCommand <$> key <*> integer <*> reply <* newline

flushAll :: Parser Command
flushAll = stringCI "flush_all" >> FlushAllCommand <$> optionMaybe integer <*> reply <* newline

get :: Parser Command
get = stringCI "get" >> GetCommand <$> many' key <* newline

gets :: Parser Command
gets = stringCI "gets" >> GetsCommand <$> many' key <* newline

stats :: Parser Command
stats = stringCI "stats" >> StatisticsCommand <$> optionMaybe statisticsOption <* newline

version :: Parser Command
version = stringCI "version" >> newline >> return VersionCommand

verbosity :: Parser Command
verbosity = stringCI "verbosity" >> VerbosityCommand <$> verbosityLevel <* newline

quit :: Parser Command
quit = stringCI "quit" >> newline >> return QuitCommand

key :: Parser Key
key = skipSpace1 >> takeWhile1 isPrintable

flags :: Parser Flags
flags = skipSpace1 >> decimal

exptime :: Parser Exptime
exptime = skipSpace1 >> decimal

bytes :: Parser Bytes
bytes = skipSpace1 >> decimal

reply :: Parser Reply
reply = skipSpace >> option True (stringCI "noreply" >> return False)

casUnique :: Parser CasUnique
casUnique = skipSpace1 >> decimal

time :: Parser Time
time = skipSpace1 >> decimal

integer :: Parser Integer
integer = skipSpace1 >> decimal

int :: Parser Int
int = skipSpace1 >> decimal

statisticsOption :: Parser StatisticsOption
statisticsOption = skipSpace1 >> choice
                     [ stringCI "items" >> return StatisticsOptionItems
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
