module Todo.Cli.Render.TaskId
  ( render,
  )
where

import System.Console.Pretty qualified as Pretty
import Todo.Cli.Prelude
import Todo.Cli.Render.Utils (ColorSwitch (ColorOff, ColorOn))
import Todo.Cli.Render.Utils qualified as Render.Utils
import Todo.Data.TaskId (TaskId)

-- | Renders a TaskId.
render :: ColorSwitch -> TaskId -> Builder
render ColorOff = displayBuilder . (.unTaskId)
render ColorOn = Render.Utils.colorBuilder Pretty.Blue . (.unTaskId)
