{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module: Domain
-- Pure domain logic for todos. No IO, no ID generation, no clock access -
-- callers inject 'UTCTime' and 'UUID' so this module stays deterministic
-- and trivially testable.
module Domain
  ( Todo (..),
    TodoStatus (..),
    completeTodo,
    createTodo,
    emptyTodoMap,
    getTodo,
    insertTodo,
    deleteTodo,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data TodoStatus = Pending | Completed
  deriving (Show, Eq, Generic, Enum, Bounded)

instance ToJSON TodoStatus

instance FromJSON TodoStatus

data Todo = Todo
  { title :: T.Text,
    status :: TodoStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON Todo

instance FromJSON Todo

emptyTodoMap :: Map UUID Todo
emptyTodoMap = M.empty

-- Pass UTCTime created/updated at to keep function pure
createTodo :: UTCTime -> T.Text -> Todo
createTodo now title =
  Todo
    { title,
      status = Pending,
      createdAt = now,
      updatedAt = now
    }

-- The caller of `insertTodo` owns the UUID (used as the key in the Map) -
-- this keeps `insertTodo` pure.
insertTodo :: UUID -> Todo -> Map UUID Todo -> Map UUID Todo
insertTodo = M.insert

getTodo :: UUID -> Map UUID Todo -> Maybe Todo
getTodo = M.lookup

deleteTodo :: UUID -> Map UUID Todo -> Map UUID Todo
deleteTodo = M.delete

completeTodo :: UTCTime -> UUID -> Map UUID Todo -> Map UUID Todo
completeTodo now = M.adjust (\t -> t {status = Completed, updatedAt = now})
