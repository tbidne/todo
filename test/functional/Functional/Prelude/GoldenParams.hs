{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Avoid lambda" -}

module Functional.Prelude.GoldenParams
  ( GoldenParams (..),
  )
where

import Test.Tasty (TestName)
import Todo.Prelude

-- | Params for golden tests.
data GoldenParams = MkGoldenParams
  { -- | The full (relative) path to the golden file we wish to test.
    -- If Nothing, we use examples/index.json.
    indexPath :: Maybe OsPath,
    -- | Golden runner. Defaults to runTodo.
    runner :: Maybe (List String -> IO Text),
    -- | Tasty test description.
    testDesc :: TestName,
    -- | Directory name where the input/output directories exist relative
    -- to each test module. E.g. 'Delete' referencing
    -- test/functional/Functional/Delete.
    dataDir :: OsPath,
    -- | Directory name relative to the test itself. This should be the
    -- unique function name of the test e.g. testSetStatus.
    testDirName :: OsPath,
    -- | CLI args.
    args :: List String,
    -- | If true, runs a 'list' command after the primary command, and
    -- concatentates the two results together.
    runList :: Bool
  }

instance
  (k ~ A_Lens, a ~ Maybe OsPath, b ~ Maybe OsPath) =>
  LabelOptic "indexPath" k GoldenParams GoldenParams a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkGoldenParams _x1 _x2 _x3 _x4 _x5 _x6 _x7) ->
          fmap
            (\y -> MkGoldenParams y _x2 _x3 _x4 _x5 _x6 _x7)
            (f _x1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe (List String -> IO Text), b ~ Maybe (List String -> IO Text)) =>
  LabelOptic "runner" k GoldenParams GoldenParams a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkGoldenParams _x1 _x2 _x3 _x4 _x5 _x6 _x7) ->
          fmap
            (\y -> MkGoldenParams _x1 y _x3 _x4 _x5 _x6 _x7)
            (f _x2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ TestName, b ~ TestName) =>
  LabelOptic "testDesc" k GoldenParams GoldenParams a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkGoldenParams _x1 _x2 _x3 _x4 _x5 _x6 _x7) ->
          fmap
            (\y -> MkGoldenParams _x1 _x2 y _x4 _x5 _x6 _x7)
            (f _x3)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ OsPath, b ~ OsPath) =>
  LabelOptic "dataDir" k GoldenParams GoldenParams a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkGoldenParams _x1 _x2 _x3 _x4 _x5 _x6 _x7) ->
          fmap
            (\y -> MkGoldenParams _x1 _x2 _x3 y _x5 _x6 _x7)
            (f _x4)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ OsPath, b ~ OsPath) =>
  LabelOptic "testDirName" k GoldenParams GoldenParams a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkGoldenParams _x1 _x2 _x3 _x4 _x5 _x6 _x7) ->
          fmap
            (\y -> MkGoldenParams _x1 _x2 _x3 _x4 y _x6 _x7)
            (f _x5)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ List String, b ~ List String) =>
  LabelOptic "args" k GoldenParams GoldenParams a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkGoldenParams _x1 _x2 _x3 _x4 _x5 _x6 _x7) ->
          fmap
            (\y -> MkGoldenParams _x1 _x2 _x3 _x4 _x5 y _x7)
            (f _x6)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "runList" k GoldenParams GoldenParams a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkGoldenParams _x1 _x2 _x3 _x4 _x5 _x6 _x7) ->
          fmap
            (\y -> MkGoldenParams _x1 _x2 _x3 _x4 _x5 _x6 y)
            (f _x7)
  {-# INLINE labelOptic #-}
