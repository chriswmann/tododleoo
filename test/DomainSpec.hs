{-# LANGUAGE OverloadedStrings #-}

module DomainSpec (spec) where

import Data.Maybe (fromJust)
import Data.Time (UTCTime (..), fromGregorian)
import Data.UUID (fromWords64)
import Domain (Todo (..), TodoStatus (..), createTodo, emptyTodoMap, getTodo, insertTodo)
import Test.Hspec (Spec, describe, it, shouldBe)

referenceTime :: UTCTime
referenceTime = UTCTime (fromGregorian 2026 05 15) 0

spec :: Spec
spec = do
  describe "createTodo" $ do
    it "uses the title that was passed in" $ do
      let todo = createTodo referenceTime "Title"
      title todo `shouldBe` "Title"

    it "uses the reference time that was passed in for createdAt" $ do
      let todo = createTodo referenceTime "Title"
      createdAt todo `shouldBe` referenceTime

    it "uses the referenceTime time that was passed in for updatedAt" $ do
      let todo = createTodo referenceTime "Title"
      updatedAt todo `shouldBe` referenceTime

    it "creates the todo with todoStatus Pending" $ do
      let todo = createTodo referenceTime "Title"
      status todo `shouldBe` Pending

  describe "insertTodo" $ do
    it "inserts the todo into the todo store" $ do
      let anyId = fromWords64 0 1
      let todo = createTodo referenceTime "Title"
      let store = emptyTodoMap
      let storeWithInsertedToto = insertTodo anyId todo store
      getTodo anyId storeWithInsertedToto `shouldBe` Just todo

    it "overwrites an existing entry for the same key" $ do
      let knownId = fromWords64 0 1
      let firstTodo = createTodo referenceTime "First todo"
      let store = emptyTodoMap
      let storeWithFirstTodo = insertTodo knownId firstTodo store
      let secondTodo = createTodo referenceTime "Second todo"
      let storeWithSecondtodo = insertTodo knownId secondTodo storeWithFirstTodo
      let retrievedTodo = fromJust $ getTodo knownId storeWithSecondtodo
      title retrievedTodo `shouldBe` "Second todo"

    it "allows a new todo with unique ID to be inserted without affecting other todos in the store" $ do
      let anyIdOne = fromWords64 0 1
      let anyIdTwo = fromWords64 0 2

      let todoOne = createTodo referenceTime "First todo"
      let todoTwo = createTodo referenceTime "Second todo"

      let store = emptyTodoMap
      let storeWithFirstTodo = insertTodo anyIdOne todoOne store
      let storeWithSecondTodo = insertTodo anyIdTwo todoTwo storeWithFirstTodo
      let retrievedFirstTodo = fromJust $ getTodo anyIdOne storeWithSecondTodo
      retrievedFirstTodo `shouldBe` todoOne
