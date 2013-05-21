{
module Network.Memcached.Parser (parseCommand) where

import Network.Memcached.Command
import Network.Memcached.Token

parseError :: [Token] -> a
parseError _ = error "Parse error"
}

%name parseCommand
%tokentype { Token }
%error { parseError }

%token
  {- The list of possible commands -}
  set         { TokenSet }
  add         { TokenAdd }
  replace     { TokenReplace }
  append      { TokenAppend }
  prepend     { TokenPrepend }
  cas         { TokenCas }
  get         { TokenGet }
  gets        { TokenGets }
  delete      { TokenDelete }
  incr        { TokenIncr }
  decr        { TokenDecr }
  stats       { TokenStats }
  flush_all   { TokenFlushAll }
  version     { TokenVersion }
  verbosity   { TokenVerbosity }
  quit        { TokenQuit }

  {- Parameter tokens -}
  key         { TokenKey $$ }
  integer     { TokenInteger $$ }
  noreply     { TokenNoReply }
  items       { TokenItems }
  slabs       { TokenSlabs }
  sizes       { TokenSizes }

%%

Command :: { Command }
        : ModCommand           { $1 True }
        | ModCommand noreply   { $1 False }
        | RepCommand           { $1 }
        | quit                 { QuitCommand }

{- Commands that modify state. -}
ModCommand :: { Bool -> Command }
           : set       key flags exptime bytes              { SetCommand $2 $3 $4 $5 }
           | add       key flags exptime bytes              { AddCommand $2 $3 $4 $5 }
           | replace   key flags exptime bytes              { ReplaceCommand $2 $3 $4 $5 }
           | append    key flags exptime bytes              { AppendCommand $2 $3 $4 $5 }
           | prepend   key flags exptime bytes              { PrependCommand $2 $3 $4 $5 }
           | cas       key flags exptime bytes cas_unique   { CasCommand $2 $3 $4 $5 $6 }
           | delete    key                                  { DeleteCommand $2 Nothing }
           | delete    key time                             { DeleteCommand $2 $ Just $3 }
           | incr      key value                            { IncrementCommand $2 $3 }
           | decr      key value                            { DecrementCommand $2 $3 }
           | flush_all                                      { FlushAllCommand Nothing }
           | flush_all delay                                { FlushAllCommand $ Just $2 }

{- Commands that only report state. -}
RepCommand :: { Command }
           : get       keys              { GetCommand $2 }
           | gets      keys              { GetsCommand $2 }
           | stats                       { StatisticsCommand Nothing }
           | stats     items             { StatisticsCommand $ Just StatisticsOptionItems }
           | stats     slabs             { StatisticsCommand $ Just StatisticsOptionSlabs }
           | stats     sizes             { StatisticsCommand $ Just StatisticsOptionSizes }
           | version                     { VersionCommand }
           | verbosity verbosity_level   { VerbosityCommand $2 }

flags :: { Integer }
      : integer              { $1 }

exptime :: { Integer }
        : integer            { $1 }

bytes :: { Integer }
      : integer              { $1 }

cas_unique :: { Integer }
           : integer         { $1 }

time :: { Integer }
     : integer               { $1 }

value :: { Integer }
      : integer              { $1 }

delay :: { Integer }
      : integer              { $1 }

keys :: { [String] }
     : key                   { [$1] }
     | keys key              { $2 : $1 }

verbosity_level :: { Integer }
                : integer    { $1 }
