{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
Module: Domain.Internal
Unsafe internals of the todo domain. This module exposes the raw
'TodoTitle', 'Todo' and 'Store' constructors, bypassing the smart
constructor 'parseTodoTitle' and its invariants. Import it only where you
are prepared to uphold those invariants yourself - chiefly the test suite.
Production code should import 'Domain' instead.
-}
module Domain.Internal (
  Todo (..),
  TitleError (..),
  TodoId (..),
  TodoStatus (..),
  TodoTitle (..),
  Store (..),
  TodoMap,
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
  overTodos,
  parseTodoId,
  parseTodoTitle,
)
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Text.Read (readMaybe)

data TodoStatus = Pending | Completed
  deriving (Show, Eq, Generic, Enum, Bounded)

instance ToJSON TodoStatus

instance FromJSON TodoStatus

newtype TodoId = TodoId {unTodoId :: Natural}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

parseTodoId :: String -> Maybe TodoId
parseTodoId sid = TodoId <$> readMaybe sid

displayTodoId :: TodoId -> String
displayTodoId tid = show (unTodoId tid)

nextTodoId :: TodoId -> TodoId
nextTodoId tid = TodoId (unTodoId tid + 1)

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
  { title :: TodoTitle
  , status :: TodoStatus
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON Todo

instance FromJSON Todo

type TodoMap = Map TodoId Todo

data Store = Store
  { todos :: TodoMap
  , nextId :: TodoId -- Deriving ToJSON and FromJSON for `nextId` ensures we don't reuse 'deleted' IDs after serialisation
  }
  deriving (Show, Eq, Generic)

instance ToJSON Store

instance FromJSON Store

emptyStore :: Store
emptyStore = Store{todos = M.empty, nextId = TodoId 1}

overTodos :: (TodoMap -> TodoMap) -> Store -> Store
overTodos f s = s{todos = f (todos s)}

-- Pass UTCTime created/updated at to keep function pure
createTodo :: UTCTime -> TodoTitle -> Todo
createTodo now title =
  Todo
    { title
    , status = Pending
    , createdAt = now
    , updatedAt = now
    }

insertTodo :: Todo -> Store -> (TodoId, Store)
insertTodo todo s =
  ( nextId s
  , s
      { todos = M.insert (nextId s) todo $ todos s
      , nextId = nextTodoId (nextId s)
      }
  )

getTodo :: TodoId -> Store -> Maybe Todo
getTodo todoId store = M.lookup todoId $ todos store

deleteTodo :: TodoId -> Store -> Store
deleteTodo todoId = overTodos $ M.delete todoId

completeTodo :: UTCTime -> TodoId -> Store -> Store
completeTodo now todoId = overTodos $ M.adjust markDone todoId
 where
  markDone t = case status t of
    Completed -> t
    Pending -> t{status = Completed, updatedAt = now}

getTodoStatus :: Todo -> TodoStatus
getTodoStatus = status

getTodoCreatedAt :: Todo -> UTCTime
getTodoCreatedAt = createdAt

getTodoUpdatedAt :: Todo -> UTCTime
getTodoUpdatedAt = updatedAt

getTodoTitle :: Todo -> TodoTitle
getTodoTitle = title
