{-# LANGUAGE OverloadedStrings #-}

module DomainSpec (spec) where

import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Time (UTCTime (..), addUTCTime)
import Domain (TitleError (..), TodoStatus (..), completeTodo, createTodo, emptyStore, getTodo, getTodoCreatedAt, getTodoStatus, getTodoUpdatedAt, insertTodo, parseTodoTitle)
import Domain.Internal (TodoTitle (..))
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, arbitrary, arbitraryUnicodeChar, chooseInt, counterexample, elements, forAll, property, suchThat, vectorOf, (.&&.), (===))
import Test.QuickCheck.Instances ()

genValidTitleText :: Gen T.Text
genValidTitleText = do
  len <- chooseInt (1, 256)
  if len == 1
    then T.singleton <$> nonSpace
    else do
      first <- nonSpace
      middle <- vectorOf (len - 2) arbitraryUnicodeChar
      end <- nonSpace
      pure (T.pack (first : middle ++ [end]))
  where
    nonSpace = arbitraryUnicodeChar `suchThat` (not . isSpace)

genValidTodotitle :: Gen TodoTitle
genValidTodotitle = TodoTitle <$> genValidTitleText

genValidTitleAndTime :: Gen (TodoTitle, UTCTime)
genValidTitleAndTime = do
  title <- genValidTodotitle
  now <- arbitrary
  pure (title, now)

prop_emptyTitleRejected :: Property
prop_emptyTitleRejected =
  forAll (elements ["", " ", "   ", "\t\n"]) $ \blank -> parseTodoTitle blank === Left EmptyTitle

prop_validTitleRoundTrip :: Property
prop_validTitleRoundTrip = forAll genValidTitleText $ \t -> fmap unTodoTitle (parseTodoTitle t) === Right t

prop_completingTwiceIsIdempotent :: Property
prop_completingTwiceIsIdempotent =
  forAll genValidTitleAndTime $ \(title, now) ->
    let todo = createTodo now title
        (todoId, s1) = insertTodo todo emptyStore
        firstCompleteNow = addUTCTime 1 now
        s2 = completeTodo firstCompleteNow todoId s1
        secondCompleteNow = addUTCTime 1 firstCompleteNow
        s3 = completeTodo secondCompleteNow todoId s2
     in case getTodo todoId s3 of
          Nothing -> counterexample "todo vanished from the store" (property False)
          Just completed ->
            getTodoStatus completed
              === Completed
              .&&. getTodoCreatedAt completed
                === now
              .&&. getTodoUpdatedAt completed
                === firstCompleteNow

spec :: Spec
spec = do
  describe "parseTodoTitle" $ do
    prop "rejects an empty title string" prop_emptyTitleRejected
    prop "undoTodoTitle roundtrips parseTodoTitle on valid titles" prop_validTitleRoundTrip

  describe "completeTodo" $ do
    prop "completeTodo is idempotent" prop_completingTwiceIsIdempotent
