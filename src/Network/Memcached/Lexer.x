{
module Network.Memcached.Lexer (alexScanTokens) where

import Network.Memcached.Token
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  -- Whitespace produces no tokens
  $white+     ;

  -- The list of possible commands
  set         { const TokenSet }
  add         { const TokenAdd }
  replace     { const TokenReplace }
  append      { const TokenAppend }
  prepend     { const TokenPrepend }
  cas         { const TokenCas }
  get         { const TokenGet }
  gets        { const TokenGets }
  delete      { const TokenDelete }
  incr        { const TokenIncr }
  decr        { const TokenDecr }
  stats       { const TokenStats }
  flush_all   { const TokenFlushAll }
  version     { const TokenVersion }
  verbosity   { const TokenVerbosity }
  quit        { const TokenQuit }

  -- Additional static tokens
  noreply     { const TokenNoReply }
  items       { const TokenItems }
  slabs       { const TokenSlabs }
  sizes       { const TokenSizes }

  -- Content tokens
  $digit+                         { \s -> TokenInteger . Integer $ read s }
  $alpha [$alpha $digit \_ \']*   { \s -> TokenKey s }
