{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter qualified as PW
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Cli.Functional.Delete qualified as Delete
import Test.Cli.Functional.Insert qualified as Insert
import Test.Cli.Functional.List qualified as List
import Test.Cli.Functional.Prelude
import Test.Cli.Functional.Update qualified as Update
import Test.Tasty qualified as Tasty
import Test.Tasty.Golden (DeleteOutputFile (OnPass))

main :: IO ()
main =
  Tasty.defaultMain
    $ Tasty.localOption OnPass
    $ Tasty.withResource setup teardown
    $ \env ->
      testGroup
        "CLI Functional Tests"
        [ Delete.tests env,
          Insert.tests env,
          List.tests,
          Update.tests env
        ]

setup :: IO TestEnv
setup = do
  testDir <- (\tmp -> tmp </> [osp|todo|] </> [osp|cli-tests|]) <$> PR.getTemporaryDirectory
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
