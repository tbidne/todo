-- | Main module.
--
-- @since 0.1
module Main (main) where

import System.IO (IO)
import Todo.Prelude hiding (IO)
import Todo.Runner qualified as Runner

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  setUncaughtExceptionHandlerDisplay
  Runner.runTodo
