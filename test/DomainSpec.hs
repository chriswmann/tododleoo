{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DomainSpec (spec) where

import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T
import Data.Time (UTCTime (..))
import Domain (TitleError (..), Todo, TodoStatus (..), TodoTitle, completeTodo, createTodo, deleteTodo, emptyStore, getTodo, getTodoCreatedAt, getTodoStatus, getTodoTitle, getTodoUpdatedAt, insertTodo, parseTodoTitle, unTodoTitle)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary, elements, suchThat)
import Test.QuickCheck.Instances ()

-- Define these Arbitrary instances here to keep QuickCheck dependencies out of the production module
instance Arbitrary TodoStatus where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary TodoTitle where
  arbitrary = do
    randomString <- arbitrary `suchThat` (not . null)

    case parseTodoTitle (T.pack randomString) of
      Right title -> pure title
      -- If invalid, try generating a new one
      Left _ -> arbitrary

prop_createPending :: UTCTime -> TodoTitle -> Bool
prop_createPending time title' = getTodoStatus (createTodo time title') == Pending

prop_createCreated :: UTCTime -> TodoTitle -> Bool
prop_createCreated time title' = getTodoCreatedAt (createTodo time title') == time

prop_createUpdated :: UTCTime -> TodoTitle -> Bool
prop_createUpdated time title' = getTodoUpdatedAt (createTodo time title') == time

prop_createTitle :: UTCTime -> TodoTitle -> Bool
prop_createTitle time title' = getTodoTitle (createTodo time title') == title'

prop_completeMakesCompleted :: UTCTime -> TodoTitle -> Bool
prop_completeMakesCompleted now title =
  let (i, s) = insertTodo (createTodo now title) emptyStore
      s' = completeTodo now i s
   in fmap getTodoStatus (getTodo i s') == Just Completed

spec :: Spec
spec = do
  describe "createTodo" $ do
    prop "title round trips for any text and time" prop_createTitle
    prop "createdAt round trips for any text and time" prop_createCreated
    prop "updatedAt round trips for any text and time" prop_createUpdated
    prop "status is Pending regardless of inputs" prop_createPending

  describe "the todo store" $ do
    prop "completing a todo makes it completed" prop_completeMakesCompleted
