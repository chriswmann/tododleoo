{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module DomainSpec (spec) where

import Data.Function ((&))
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian)
import Data.UUID (UUID, fromWords64)
import Domain (Todo (..), TodoStatus (..), emptyTodoMap, getTodo, insertTodo, title)
import Test.Hspec (Spec, describe, it, shouldBe)

referenceTime :: UTCTime
referenceTime = UTCTime (fromGregorian 2026 05 15) 0

makeTodo :: T.Text -> Todo
makeTodo title =
  Todo
    { title,
      status = Pending,
      createdAt = referenceTime,
      updatedAt = referenceTime
    }

anyId :: UUID
anyId = fromWords64 0 1

spec :: Spec
spec = do
  describe "createTodo" $ do
    it "uses the title that was passed in" $ do
      let todo = makeTodo "Title"
      title todo `shouldBe` "Title"

    it "uses the reference time that was passed in for createdAt" $ do
      let todo = makeTodo "Title"
      createdAt todo `shouldBe` referenceTime

    it "uses the referenceTime time that was passed in for updatedAt" $ do
      let todo = makeTodo "Title"
      updatedAt todo `shouldBe` referenceTime

    it "creates the todo with todoStatus Pending" $ do
      let todo = makeTodo "Title"
      status todo `shouldBe` Pending

  describe "insertTodo" $ do
    it "inserts the todo into the todo store" $ do
      let todo = makeTodo "Title"
      let store = emptyTodoMap
      let storeWithInsertedToto = insertTodo anyId todo store
      getTodo anyId storeWithInsertedToto `shouldBe` Just todo

    it "overwrites an existing entry for the same key" $ do
      let firstTodo = makeTodo "First todo"
      let secondTodo = makeTodo "Second todo"
      let store =
            emptyTodoMap
              & insertTodo anyId firstTodo
              & insertTodo anyId secondTodo
      let retrievedTodo = fromJust $ getTodo anyId store
      title retrievedTodo `shouldBe` "Second todo"

    it "allows a new todo with unique ID to be inserted without affecting other todos in the store" $ do
      let firstId = fromWords64 0 1
      let secondId = fromWords64 0 2

      let firstTodo = makeTodo "First todo"
      let secondTodo = makeTodo "Second todo"

      let store =
            emptyTodoMap
              & insertTodo firstId firstTodo
              & insertTodo secondId secondTodo
      let retrievedFirstTodo = fromJust $ getTodo firstId store
      retrievedFirstTodo `shouldBe` firstTodo
