{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module: Domain.Internal
-- Unsafe internals of the todo domain. This module exposes the raw
-- 'TodoTitle', 'Todo' and 'Store' constructors, bypassing the smart
-- constructor 'parseTodoTitle' and its invariants. Import it only where you
-- are prepared to uphold those invariants yourself - chiefly the test suite.
-- Production code should import 'Domain' instead.
module Domain.Internal
  ( Todo (..),
    TitleError (..),
    TodoStatus (..),
    TodoTitle (..),
    Store (..),
    TodoMap,
    completeTodo,
    createTodo,
    deleteTodo,
    emptyStore,
    getTodo,
    getTodoCreatedAt,
    getTodoStatus,
    getTodoTitle,
    getTodoUpdatedAt,
    insertTodo,
    overTodos,
    parseTodoTitle,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data TodoStatus = Pending | Completed
  deriving (Show, Eq, Generic, Enum, Bounded)

instance ToJSON TodoStatus

instance FromJSON TodoStatus

newtype TodoTitle = TodoTitle {unTodoTitle :: T.Text} deriving (Show, Eq, Generic)

instance ToJSON TodoTitle

instance FromJSON TodoTitle

data TitleError
  = EmptyTitle
  | TitleTooLong Int
  deriving (Show, Eq)

parseTodoTitle :: T.Text -> Either TitleError TodoTitle
parseTodoTitle title
  | T.null trimmedTitle = Left EmptyTitle
  | lenTrimmedTitle > 256 = Left (TitleTooLong lenTrimmedTitle)
  | otherwise = Right (TodoTitle trimmedTitle)
  where
    trimmedTitle = T.strip title
    lenTrimmedTitle = T.length trimmedTitle

data Todo = Todo
  { title :: TodoTitle,
    status :: TodoStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON Todo

instance FromJSON Todo

type TodoMap = IntMap Todo

data Store = Store
  { todos :: TodoMap,
    nextId :: Int -- Deriving ToJSON and FromJSON for `nextId` ensures we don't reuse 'deleted' IDs after serialisation
  }
  deriving (Show, Eq, Generic)

instance ToJSON Store

instance FromJSON Store

emptyStore :: Store
emptyStore = Store {todos = M.empty, nextId = 1}

overTodos :: (TodoMap -> TodoMap) -> Store -> Store
overTodos f s = s {todos = f (todos s)}

-- Pass UTCTime created/updated at to keep function pure
createTodo :: UTCTime -> TodoTitle -> Todo
createTodo now title =
  Todo
    { title,
      status = Pending,
      createdAt = now,
      updatedAt = now
    }

insertTodo :: Todo -> Store -> (Int, Store)
insertTodo todo s =
  ( nextId s,
    s
      { todos = M.insert (nextId s) todo $ todos s,
        nextId = nextId s + 1
      }
  )

getTodo :: Int -> Store -> Maybe Todo
getTodo todoId store = M.lookup todoId $ todos store

deleteTodo :: Int -> Store -> Store
deleteTodo todoId = overTodos $ M.delete todoId

completeTodo :: UTCTime -> Int -> Store -> Store
completeTodo now todoId = overTodos $ M.adjust markDone todoId
  where
    markDone t = case status t of
      Completed -> t
      Pending -> t {status = Completed, updatedAt = now}

getTodoStatus :: Todo -> TodoStatus
getTodoStatus todo = status todo

getTodoCreatedAt :: Todo -> UTCTime
getTodoCreatedAt todo = createdAt todo

getTodoUpdatedAt :: Todo -> UTCTime
getTodoUpdatedAt todo = updatedAt todo

getTodoTitle :: Todo -> TodoTitle
getTodoTitle todo = title todo
