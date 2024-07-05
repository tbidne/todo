-- | Main module.
--
-- @since 0.1
module Main (main) where

import System.Console.Haskeline qualified as H
import System.IO (IO)
import Todo.AppT (runAppIO)
import Todo.Prelude hiding (IO)
import Todo.Runner qualified as Runner

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  setUncaughtExceptionHandlerDisplay
  H.runInputT H.defaultSettings $ runAppIO Runner.runTodo
