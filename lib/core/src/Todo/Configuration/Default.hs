module Todo.Configuration.Default
  ( Default (..),
    fromDefault,
    (<.>),
  )
where

import Todo.Prelude

class Default a where
  def :: a

fromDefault :: (Default a) => Maybe a -> a
fromDefault Nothing = def
fromDefault (Just x) = x

(<.>) :: (Default a) => Maybe a -> Maybe a -> a
x <.> y = fromDefault (x <|> y)
