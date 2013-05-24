import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Network.Memcached.Command
import Network.Memcached.Lexer (alexScanTokens)
import Network.Memcached.Parser (parseCommand)
import qualified Network.Memcached.Types as Mem

main :: IO ()
main = do
  runStateT loop Map.empty
  return ()

loop :: Mem.MemStateM ()
loop = do
  command <- fmap (parseCommand . alexScanTokens) $ liftIO getLine
  state <- get
  let (msg, newState) = apply command state
  put newState
  when (showMsg command) $ liftIO $ putStrLn msg
  unless (isQuit command) loop
