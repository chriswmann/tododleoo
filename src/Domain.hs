{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Domain
  ( Todo(..)
  , TodoStatus(..)
  , completeTodo
  , createTodo
  , emptyTodoMap
  , getTodo
  , insertTodo
  , deleteTodo
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Time (UTCTime)

data TodoStatus = Pending | Completed
  deriving (Show, Eq, Generic)

instance ToJSON TodoStatus
instance FromJSON TodoStatus

data Todo = Todo
  { title :: T.Text
  , status :: TodoStatus
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON Todo
instance FromJSON Todo

emptyTodoMap :: Map UUID Todo
emptyTodoMap = M.empty

createTodo :: UTCTime -> T.Text -> Todo
createTodo now title = Todo {
    title
    , status = Pending
    , createdAt = now
    , updatedAt = now
  }

insertTodo :: UUID -> Todo -> Map UUID Todo -> Map UUID Todo
insertTodo = M.insert 

getTodo :: UUID -> Map UUID Todo -> Maybe Todo
getTodo = M.lookup

deleteTodo :: UUID -> Map UUID Todo -> Map UUID Todo
deleteTodo = M.delete

completeTodo :: UTCTime -> UUID -> Map UUID Todo -> Map UUID Todo
completeTodo now = M.adjust (\t -> t {status = Completed, updatedAt = now})
