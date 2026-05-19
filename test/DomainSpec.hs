{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DomainSpec (spec) where

import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Time (UTCTime (..))
import Domain (Todo (..), TodoStatus (..), completeTodo, createTodo, deleteTodo, emptyStore, getTodo, insertTodo)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Instances ()

-- Define these Arbitrary instances here to keep QuickCheck dependencies out of the production module
instance Arbitrary TodoStatus where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Todo where
  arbitrary = Todo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

prop_createPending :: UTCTime -> T.Text -> Bool
prop_createPending time title' = status (createTodo time title') == Pending

prop_createCreated :: UTCTime -> T.Text -> Bool
prop_createCreated time title' = createdAt (createTodo time title') == time

prop_createUpdated :: UTCTime -> T.Text -> Bool
prop_createUpdated time title' = updatedAt (createTodo time title') == time

prop_createTitle :: UTCTime -> T.Text -> Bool
prop_createTitle time title' = title (createTodo time title') == title'

prop_insertThenGet :: Todo -> Bool
prop_insertThenGet todo =
  getTodo todoId store == Just todo
  where
    (todoId, store) = insertTodo todo emptyStore

prop_deleteThenGet :: Todo -> Bool
prop_deleteThenGet todo =
  isNothing $ getTodo todoId (deleteTodo todoId store)
  where
    (todoId, store) = insertTodo todo emptyStore

prop_insertTwoThenGetOne :: Todo -> Todo -> Bool
prop_insertTwoThenGetOne todo1 todo2 =
  getTodo todoId1 store2 == Just todo1
  where
    (todoId1, store1) = insertTodo todo1 emptyStore
    (_, store2) = insertTodo todo2 store1

prop_insertTwoThenDeleteOneThenGetTheOther :: Todo -> Todo -> Bool
prop_insertTwoThenDeleteOneThenGetTheOther todo1 todo2 =
  getTodo todoId1 (deleteTodo todoId2 store2) == Just todo1
  where
    (todoId1, store1) = insertTodo todo1 emptyStore
    (todoId2, store2) = insertTodo todo2 store1

--
prop_insertThenComplete :: UTCTime -> Todo -> Bool
prop_insertThenComplete now todo =
  getTodo todoId storeCompleteTodo == Just todo {status = Completed, updatedAt = now}
  where
    (todoId, storeNewTodo) = insertTodo todo emptyStore
    storeCompleteTodo = completeTodo now todoId storeNewTodo

prop_insertThenDeleteThenInsert :: Todo -> Todo -> Bool
prop_insertThenDeleteThenInsert todo1 todo2 =
  todoId2 == 2
  where
    (todoId1, store1) = insertTodo todo1 emptyStore
    store2 = deleteTodo todoId1 store1
    (todoId2, _) = insertTodo todo2 store2

spec :: Spec
spec = do
  describe "createTodo" $ do
    prop "title round trips for any text and time" prop_createTitle
    prop "createdAt round trips for any text and time" prop_createCreated
    prop "updatedAt round trips for any text and time" prop_createUpdated
    prop "status is Pending regardless of inputs" prop_createPending

  describe "the todo store" $ do
    prop "an inserted todo is retrievable by its key" prop_insertThenGet
    prop "a deleted todo cannot be retrieved by its key" prop_deleteThenGet
    prop "inserting a todo does not prevent retrieval of another already in the store" prop_insertTwoThenGetOne

    prop "deleting a todo does not prevent retrieval of another already in the store" prop_insertTwoThenDeleteOneThenGetTheOther
    prop "completing an inserted todo sets the status and updatedAt, preserves other fields" prop_insertThenComplete
    prop "a deleted todo's ID is not reused" prop_insertThenDeleteThenInsert
