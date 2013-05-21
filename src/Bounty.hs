import Control.Monad
import Network.Memcached.Lexer (alexScanTokens)
import Network.Memcached.Parser (parseCommand)
import Network.Memcached.Command

isQuit :: Command -> Bool
isQuit QuitCommand = True
isQuit _ = False

main :: IO ()
main = do
  line <- getLine
  let command = parseCommand $ alexScanTokens line
  unless (isQuit command) $ do
    print command
    main
