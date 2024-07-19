{-# LANGUAGE UndecidableInstances #-}

module Todo.Configuration.Data.RevSort
  ( RevSort (..),
  )
where

import Todo.Configuration.Default (Default (def))
import Todo.Prelude

data RevSort
  = RevSortOff
  | RevSortOn
  deriving stock (Eq, Show)

instance
  (k ~ An_Iso, a ~ Bool, b ~ Bool) =>
  LabelOptic "boolIso" k RevSort RevSort a b
  where
  labelOptic =
    iso
      (\case RevSortOff -> False; RevSortOn -> True)
      (\case False -> RevSortOff; True -> RevSortOn)

instance Default RevSort where
  def = RevSortOff
