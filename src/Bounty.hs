{-# LANGUAGE OverloadedStrings #-}
-- import Control.Applicative
-- import Control.Monad
-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.State
import Data.Attoparsec
-- import Data.Conduit
-- import qualified Data.Conduit.List as CL
import Network.Memcached.Parser
-- import Network.Memcached.Command
-- import qualified Network.Memcached.Types as Mem

main :: IO ()
main = print $ parseOnly commands "set asdf 1 2 3\r\nadd sdfg 4 5 6 noreply\r\nversion \r\nquit\t \t\r\n"

-- main = do
--   runStateT loop Map.empty
--   return ()

-- loop :: Mem.MemStateM ()
-- loop = do
--   command <- fmap (parseCommand . alexScanTokens) $ liftIO getLine
--   state <- get
--   let (msg, newState) = apply command state
--   put newState
--   when (showMsg command) $ liftIO $ putStrLn msg
--   unless (isQuit command) loop
