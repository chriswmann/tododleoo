{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DomainSpec (spec) where

import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Time (UTCTime (..))
import Data.UUID (UUID)
import Domain (Todo (..), TodoStatus (..), completeTodo, createTodo, deleteTodo, getTodo, insertTodo)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Property, arbitrary, elements, (==>))
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

prop_insertThenGet :: UUID -> Todo -> Map UUID Todo -> Bool
prop_insertThenGet k v m =
  getTodo k store == Just v
  where
    store = m & insertTodo k v

prop_deleteThenGet :: UUID -> Todo -> Map UUID Todo -> Bool
prop_deleteThenGet k v =
  isNothing . getTodo k . deleteTodo k . insertTodo k v

prop_insertTwoThenGetOne :: UUID -> Todo -> UUID -> Todo -> Map UUID Todo -> Property
prop_insertTwoThenGetOne k1 v1 k2 v2 m =
  k1 /= k2 ==> getTodo k1 store == Just v1
  where
    store = m & insertTodo k1 v1 & insertTodo k2 v2

prop_insertTwoThenDeleteOneThenGetTheOther :: UUID -> Todo -> UUID -> Todo -> Map UUID Todo -> Property
prop_insertTwoThenDeleteOneThenGetTheOther k1 v1 k2 v2 m =
  k1 /= k2 ==> getTodo k2 store == Just v2
  where
    store = m & insertTodo k1 v1 & insertTodo k2 v2 & deleteTodo k1

prop_insertThenComplete :: UTCTime -> UUID -> Todo -> Map UUID Todo -> Bool
prop_insertThenComplete now todoId todo m =
  getTodo todoId store == Just todo {status = Completed, updatedAt = now}
  where
    store = m & insertTodo todoId todo & completeTodo now todoId

prop_insertOverwrite :: UUID -> Todo -> Todo -> Map UUID Todo -> Bool
prop_insertOverwrite todoId todo1 todo2 m =
  getTodo todoId store == Just todo2
  where
    store = m & insertTodo todoId todo1 & insertTodo todoId todo2

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
    prop "the second insert at a key wins" prop_insertOverwrite
