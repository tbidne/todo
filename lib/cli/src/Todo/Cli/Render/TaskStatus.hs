module Todo.Cli.Render.TaskStatus
  ( render,
  )
where

import System.Console.Pretty qualified as Pretty
import Todo.Cli.Prelude
import Todo.Cli.Render.Utils (ColorSwitch (ColorOff, ColorOn))
import Todo.Cli.Render.Utils qualified as Render.Utils
import Todo.Data.TaskStatus
  ( TaskStatus
      ( Blocked,
        Completed,
        InProgress,
        NotStarted
      ),
  )
import Todo.Data.TaskStatus qualified as Status

-- | Renders to Builder.
render :: ColorSwitch -> TaskStatus -> Builder
render ColorOff Completed = "completed"
render ColorOff InProgress = "in-progress"
render ColorOff (Blocked tids) =
  displayBuilder $ "blocked: " <> Status.blockersToText tids
render ColorOff NotStarted = "not-started"
render ColorOn Completed = Render.Utils.colorBuilder Pretty.Green "completed"
render ColorOn InProgress = Render.Utils.colorBuilder Pretty.Yellow "in-progress"
render ColorOn (Blocked tids) =
  Render.Utils.colorBuilder Pretty.Red $ "blocked: " <> Status.blockersToText tids
render ColorOn NotStarted = Render.Utils.colorBuilder Pretty.Cyan "not-started"
