{- |
Module: Domain
Pure domain logic for todos. No IO, no ID generation, no clock access -
callers inject 'UTCTime' and 'Int' so this module stays deterministic
and trivially testable.

This is the safe public surface: 'TodoTitle', 'Todo' and 'Store' are
exported as opaque types, so the only way to build a 'TodoTitle' is via the
smart constructor 'parseTodoTitle'. Tests that need the raw constructors
should import "Domain.Internal" instead.
-}
module Domain (
  Todo,
  TitleError (..),
  TodoId,
  TodoStatus (..),
  TodoTitle,
  Store,
  completeTodo,
  createTodo,
  deleteTodo,
  displayTodoId,
  emptyStore,
  getTodo,
  getTodoCreatedAt,
  getTodoStatus,
  getTodoTitle,
  getTodoUpdatedAt,
  insertTodo,
  parseTodoId,
  parseTodoTitle,
  unTodoTitle,
)
where

import Domain.Internal
