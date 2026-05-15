{-# LANGUAGE OverloadedStrings #-}

module DomainSpec (spec) where

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
