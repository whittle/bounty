module Network.Memcached.Command
       ( Command(..)
       , StatisticsOption(..)
       ) where

data Command = SetCommand String Integer Integer Integer Bool
             | AddCommand String Integer Integer Integer Bool
             | ReplaceCommand String Integer Integer Integer Bool
             | AppendCommand String Integer Integer Integer Bool
             | PrependCommand String Integer Integer Integer Bool
             | CasCommand String Integer Integer Integer Integer Bool
             | DeleteCommand String (Maybe Integer) Bool
             | IncrementCommand String Integer Bool
             | DecrementCommand String Integer Bool
             | FlushAllCommand (Maybe Integer) Bool
             | GetCommand [String]
             | GetsCommand [String]
             | StatisticsCommand (Maybe StatisticsOption)
             | VersionCommand
             | VerbosityCommand Integer
             | QuitCommand
             deriving Show

data StatisticsOption = StatisticsOptionItems
                      | StatisticsOptionSlabs
                      | StatisticsOptionSizes
                      deriving Show
