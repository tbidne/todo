module Todo.Cli.Render.TaskPriority
  ( render,
  )
where

import System.Console.Pretty qualified as Pretty
import Todo.Cli.Prelude
import Todo.Cli.Render.Utils (ColorSwitch (ColorOff, ColorOn))
import Todo.Cli.Render.Utils qualified as Render.Utils
import Todo.Data.TaskPriority (TaskPriority (High, Low, Normal))

render :: ColorSwitch -> TaskPriority -> Builder
render ColorOff Low = "low"
render ColorOff Normal = "normal"
render ColorOff High = "high"
render ColorOn Low = Render.Utils.colorBuilder Pretty.Yellow "low"
render ColorOn Normal = Render.Utils.colorBuilder Pretty.Cyan "normal"
render ColorOn High = Render.Utils.colorBuilder Pretty.Red "high"
