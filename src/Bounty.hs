import Network.Memcached.Lexer (alexScanTokens)
import Network.Memcached.Parser (parseCommand)

main :: IO ()
main = do
  getLine >>= print . parseCommand . alexScanTokens
  main
