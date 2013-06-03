module Bounty.Options
       ( Settings(..)
       , options
       ) where

import qualified Data.Conduit.Network as N
import Options.Applicative

data Settings = Settings { port :: Int
                         , hostPreference :: N.HostPreference
                         } deriving (Eq, Show)

options :: ParserInfo Settings
options = info (helper <*> settings)
            (  fullDesc
            <> progDesc "Run the bounty server"
            <> header "bounty - a drop-in replacement for memcached"
            )

settings :: Parser Settings
settings = Settings
             <$> option
               (  short 'p'
               <> value 22122
               <> showDefault
               <> metavar "<num>"
               <> help "Listen on TCP port <num>."
               )
             <*> pure N.HostAny
