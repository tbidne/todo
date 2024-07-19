{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Integration.Misc (tests) where

import Data.Set.NonEmpty qualified as NESet
import Integration.Prelude
import Todo.Configuration.Core
  ( CoreConfig
      ( MkCoreConfig,
        colorSwitch,
        index,
        unicodeSwitch
      ),
  )
import Todo.Configuration.Data.Command (Command (CmdList))
import Todo.Configuration.Data.RevSort
  ( RevSort (RevSortOff, RevSortOn),
  )
import Todo.Configuration.Merged (Merged (MkMerged, command, coreConfig))
import Todo.Data.Sorted (SortType (SortPriorityStatus))
import Todo.Data.Task
  ( SingleTask
      ( MkSingleTask,
        deadline,
        description,
        priority,
        status,
        taskId
      ),
    SomeTask (SomeTaskGroup, SomeTaskSingle),
    TaskGroup (MkTaskGroup, priority, status, subtasks, taskId),
  )
import Todo.Data.TaskPriority (TaskPriority (High, Low, Normal))
import Todo.Data.TaskStatus
  ( TaskStatus (Blocked, Completed, InProgress, NotStarted),
  )
import Todo.Index.Internal (Index (UnsafeIndex))
import Todo.Render.Utils
  ( ColorSwitch (ColorOff, ColorOn),
    UnicodeSwitch (UnicodeOff, UnicodeOn),
  )

tests :: TestTree
tests =
  testGroup
    "Misc"
    [ testXdg,
      testExample,
      testCliOverridesToml,
      testTomlUsesMapName,
      testCliOverridesTomlMapName,
      testUsesTomlListConfig
    ]

testXdg :: TestTree
testXdg = testHedgehogOne "Reads Xdg" "testXdg" $ do
  -- This tests reading both xdg config and tasks.
  result <- liftIO $ runGetConfig args
  expected === result
  where
    args = ["list"]
    expected =
      MkMerged
        { coreConfig =
            MkCoreConfig
              { colorSwitch = ColorOff,
                index =
                  UnsafeIndex
                    [ SomeTaskSingle $ MkSingleTask Nothing Nothing High Completed "xdg_task"
                    ]
                    ( [osp|test|]
                        </> [osp|integration|]
                        </> [osp|toml|]
                        </> [osp|todo|]
                        </> [osp|index.json|]
                    ),
                unicodeSwitch = UnicodeOff
              },
          command = CmdList Nothing RevSortOff
        }

testExample :: TestTree
testExample = testHedgehogOne "Reads example config" "testExample" $ do
  result <- liftIO $ runGetConfig args
  expected === result
  where
    args =
      [ "--config-path",
        exampleConfigFilePath,
        "list"
      ]
    expected =
      MkMerged
        { coreConfig =
            MkCoreConfig
              { colorSwitch = ColorOff,
                index =
                  UnsafeIndex
                    expectedTaskList
                    ([osp|examples|] </> [osp|index.json|]),
                unicodeSwitch = UnicodeOff
              },
          command = CmdList Nothing RevSortOff
        }

    expectedTaskList =
      [ SomeTaskSingle
          MkSingleTask
            { deadline = Nothing,
              description = Nothing,
              priority = Low,
              status = NotStarted,
              taskId = "haircut"
            },
        SomeTaskSingle
          MkSingleTask
            { deadline = Just (unsafeParseTimestamp "2024-04-10"),
              description = Nothing,
              priority = Normal,
              status = Completed,
              taskId = "walk_dog"
            },
        SomeTaskGroup
          MkTaskGroup
            { priority = Nothing,
              status = Just (Blocked (NESet.fromList ("<fix_car>" :| ["store closed", "<paycheck>"]))),
              subtasks =
                [ SomeTaskSingle
                    MkSingleTask
                      { deadline = Nothing,
                        description = Nothing,
                        priority = Normal,
                        status = NotStarted,
                        taskId = "apples"
                      },
                  SomeTaskSingle
                    MkSingleTask
                      { deadline = Nothing,
                        description = Nothing,
                        priority = High,
                        status = NotStarted,
                        taskId = "bananas"
                      }
                ],
              taskId = "groceries"
            },
        SomeTaskSingle
          MkSingleTask
            { deadline = Just (unsafeParseTimestamp "2024-06-22 12:15:00 UTC"),
              description = Just "Car needs brakes fixed",
              priority = High,
              status = NotStarted,
              taskId = "fix_car"
            },
        SomeTaskSingle
          MkSingleTask
            { deadline = Just (unsafeParseTimestamp "2024-08-12 12:00:00"),
              description = Nothing,
              priority = High,
              status = InProgress,
              taskId = "paycheck"
            },
        SomeTaskGroup
          MkTaskGroup
            { priority = Just Low,
              status = Nothing,
              subtasks =
                [ SomeTaskSingle
                    MkSingleTask
                      { deadline = Nothing,
                        description = Nothing,
                        priority = Normal,
                        status =
                          Blocked (NESet.fromList ("<groceries>" :| [])),
                        taskId = "pack_bananas"
                      },
                  SomeTaskGroup
                    MkTaskGroup
                      { priority = Just Normal,
                        status = Nothing,
                        subtasks =
                          [ SomeTaskSingle
                              MkSingleTask
                                { deadline = Nothing,
                                  description = Just "Get cleats",
                                  priority = Normal,
                                  status =
                                    Blocked (NESet.fromList ("<fix_car>" :| [])),
                                  taskId = "cleats"
                                },
                            SomeTaskSingle
                              MkSingleTask
                                { deadline = Nothing,
                                  description = Nothing,
                                  priority = Normal,
                                  status =
                                    Blocked (NESet.fromList ("<fix_car>" :| [])),
                                  taskId = "ball"
                                }
                          ],
                        taskId = "equipment"
                      }
                ],
              taskId = "soccer_match"
            },
        SomeTaskGroup
          MkTaskGroup
            { priority = Nothing,
              status = Nothing,
              subtasks = Empty,
              taskId = "empty_group"
            }
      ]

testCliOverridesToml :: TestTree
testCliOverridesToml = testHedgehogOne "CLI overrides TOML" "testCliOverridesToml" $ do
  result <- liftIO $ runGetConfig args
  expected === result
  where
    args =
      [ "--color",
        "off",
        "--unicode",
        "off",
        "--index-path",
        "examples" `cfp` "index2.json",
        "list"
      ]
    expected =
      MkMerged
        { coreConfig =
            MkCoreConfig
              { colorSwitch = ColorOff,
                index =
                  UnsafeIndex
                    [SomeTaskSingle $ MkSingleTask Nothing Nothing Normal NotStarted "a_task"]
                    ([osp|examples|] </> [osp|index2.json|]),
                unicodeSwitch = UnicodeOff
              },
          command = CmdList Nothing RevSortOff
        }

testTomlUsesMapName :: TestTree
testTomlUsesMapName = testHedgehogOne desc "testTomlUsesMapName" $ do
  result <- liftIO $ runGetConfig args
  expected === result
  where
    desc = "Uses toml map name"
    args =
      [ "--config-path",
        noPathConfigFilePath,
        "list"
      ]
    expected =
      MkMerged
        { coreConfig =
            MkCoreConfig
              { colorSwitch = ColorOn,
                index = UnsafeIndex [] (tomlOsPath </> [osp|empty.json|]),
                unicodeSwitch = UnicodeOn
              },
          command = CmdList Nothing RevSortOff
        }

testCliOverridesTomlMapName :: TestTree
testCliOverridesTomlMapName = testHedgehogOne desc "testCliOverridesTomlMapName" $ do
  result <- liftIO $ runGetConfig args
  expected === result
  where
    desc = "CLI overrides Toml map name"
    args =
      [ "--config-path",
        noPathConfigFilePath,
        "--index-name",
        "one",
        "list"
      ]
    expected =
      MkMerged
        { coreConfig =
            MkCoreConfig
              { colorSwitch = ColorOn,
                index =
                  UnsafeIndex
                    [ SomeTaskSingle $ MkSingleTask Nothing Nothing Low InProgress "one_task"
                    ]
                    (tomlOsPath </> [osp|one.json|]),
                unicodeSwitch = UnicodeOn
              },
          command = CmdList Nothing RevSortOff
        }

testUsesTomlListConfig :: TestTree
testUsesTomlListConfig = testHedgehogOne desc "testUsesTomlListConfig" $ do
  result <- liftIO $ runGetConfig args
  expected === result
  where
    desc = "Uses Toml List Config"
    args =
      [ "--config-path",
        commandsConfigFilePath,
        "list"
      ]
    expected =
      MkMerged
        { coreConfig =
            MkCoreConfig
              { colorSwitch = ColorOn,
                index = UnsafeIndex [] (tomlOsPath </> [osp|empty.json|]),
                unicodeSwitch = UnicodeOn
              },
          command = CmdList (Just SortPriorityStatus) RevSortOn
        }
