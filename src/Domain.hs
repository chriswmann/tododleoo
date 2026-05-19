{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
Module: Domain
Pure domain logic for todos. No IO, no ID generation, no clock access -
callers inject 'UTCTime' and 'Int' so this module stays deterministic
and trivially testable.
-}
module Domain (
  Todo (..),
  TodoStatus (..),
  Store,
  completeTodo,
  createTodo,
  emptyStore,
  getTodo,
  insertTodo,
  deleteTodo,
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

newtype TodoTitle = TodoTitle {unTodoTitle :: T.Text}

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
  { title :: T.Text
  , status :: TodoStatus
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON Todo

instance FromJSON Todo

type TodoMap = IntMap Todo

data Store = Store
  { todos :: TodoMap
  , nextId :: Int -- Deriving ToJSON and FromJSON for `nextId` ensures we don't reuse 'deleted' IDs after serialisation
  }
  deriving (Show, Eq, Generic)

instance ToJSON Store

instance FromJSON Store

emptyStore :: Store
emptyStore = Store{todos = M.empty, nextId = 1}

overTodos :: (TodoMap -> TodoMap) -> Store -> Store
overTodos f s = s{todos = f (todos s)}

-- Pass UTCTime created/updated at to keep function pure
createTodo :: UTCTime -> T.Text -> Todo
createTodo now title =
  Todo
    { title
    , status = Pending
    , createdAt = now
    , updatedAt = now
    }

insertTodo :: Todo -> Store -> (Int, Store)
insertTodo todo s =
  ( nextId s
  , s
      { todos = M.insert (nextId s) todo $ todos s
      , nextId = nextId s + 1
      }
  )

getTodo :: Int -> Store -> Maybe Todo
getTodo todoId store = M.lookup todoId $ todos store

deleteTodo :: Int -> Store -> Store
deleteTodo todoId = overTodos $ M.delete todoId

completeTodo :: UTCTime -> Int -> Store -> Store
completeTodo now todoId = overTodos $ M.adjust markDone todoId
 where
  markDone t = t{status = Completed, updatedAt = now}
