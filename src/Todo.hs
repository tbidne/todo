module Todo
  ( -- * Info
    List.listTasks,

    -- * Add / Remove
    Delete.deleteTask,
    Insert.insertTask,

    -- * Update
    Update.setTaskDeadline,
    Update.setTaskDescription,
    Update.setTaskId,
    Update.setTaskPriority,
    Update.setTaskStatus,
  )
where

import Todo.Command.Delete qualified as Delete
import Todo.Command.Insert qualified as Insert
import Todo.Command.List qualified as List
import Todo.Command.Update qualified as Update
