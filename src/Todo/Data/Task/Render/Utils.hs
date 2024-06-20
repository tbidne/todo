module Todo.Data.Task.Render.Utils
  ( colorBuilder,
  )
where

import System.Console.Pretty qualified as Pretty
import Todo.Prelude

colorBuilder :: Pretty.Color -> Text -> Builder
colorBuilder c = displayBuilder . Pretty.color @Text c
