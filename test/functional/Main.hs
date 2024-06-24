{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter qualified as PW
import Functional.Insert qualified as Insert
import Functional.List qualified as List
import Functional.Prelude
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty qualified as Tasty

main :: IO ()
main =
  Tasty.defaultMain
    $ Tasty.withResource setup teardown
    $ \env ->
      testGroup
        "Functional Tests"
        [ Insert.tests env,
          List.tests env
        ]

setup :: IO TestEnv
setup = do
  testDir <- (\tmp -> tmp </> [osp|todo|] </> [osp|functional|]) <$> PR.getTemporaryDirectory
  PW.createDirectoryIfMissing True testDir

  pure
    $ MkTestEnv
      { testDir
      }

teardown :: TestEnv -> IO ()
teardown env = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    fp = env.testDir
    cleanup = PW.removePathForcibly fp
    doNothing = putStrLn $ "*** Not cleaning up tmp dir: " <> show fp
