-- | Main module.
--
-- @since 0.1
module Main (main) where

import System.Console.Haskeline qualified as H
import System.IO (IO)
import Todo.Cli qualified
import Todo.Cli.AppT (runAppIO)
import Todo.Prelude hiding (IO)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  setUncaughtExceptionHandlerDisplay
  H.runInputT H.defaultSettings $ runAppIO Todo.Cli.runTodo
