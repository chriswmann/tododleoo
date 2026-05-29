{-# LANGUAGE OverloadedStrings #-}

module DomainSpec (spec) where

import Data.Char (isSpace)
import Data.List (mapAccumL)
import qualified Data.Text as T
import Data.Time (UTCTime (..), addUTCTime)
import Domain (Store, TitleError (..), Todo, TodoStatus (..), completeTodo, createTodo, emptyStore, getTodo, getTodoCreatedAt, getTodoStatus, getTodoUpdatedAt, insertTodo, parseTodoTitle)
import Domain.Internal (TodoTitle (..))
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, arbitrary, arbitraryUnicodeChar, chooseInt, counterexample, elements, forAll, forAllShrink, listOf, property, suchThat, vectorOf, (.&&.), (===))
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

genTooLongTitleText :: Gen T.Text
genTooLongTitleText = do
  len <- chooseInt (257, 4096)
  first <- nonSpace
  middle <- vectorOf (len - 2) arbitraryUnicodeChar
  end <- nonSpace
  pure (T.pack (first : middle ++ [end]))
  where
    nonSpace = arbitraryUnicodeChar `suchThat` (not . isSpace)

shrinkTooLongText :: T.Text -> [T.Text]
shrinkTooLongText text = [candidate | n <- [257 .. T.length text - 1], let candidate = T.take n text, not $ (isSpace . T.last) candidate]

genValidTodoTitle :: Gen TodoTitle
genValidTodoTitle = TodoTitle <$> genValidTitleText

genValidTitleAndTime :: Gen (TodoTitle, UTCTime)
genValidTitleAndTime = do
  title <- genValidTodoTitle
  now <- arbitrary
  pure (title, now)

insertMany :: [Todo] -> Store -> (Store, [Int])
insertMany todos store =
  mapAccumL (\s t -> let (i, s') = insertTodo t s in (s', i)) store todos

genValidTodo :: Gen Todo
genValidTodo = uncurry (flip createTodo) <$> genValidTitleAndTime

genValidTodos :: Gen [Todo]
genValidTodos = listOf genValidTodo

prop_emptyTitleRejected :: Property
prop_emptyTitleRejected =
  forAll (elements ["", " ", "   ", "\t\n"]) $ \blank -> parseTodoTitle blank === Left EmptyTitle

prop_rejectTitleTooLong :: Property
prop_rejectTitleTooLong =
  forAllShrink genTooLongTitleText shrinkTooLongText $ \t -> (parseTodoTitle t) === Left (TitleTooLong (T.length t))

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

prop_freshTodoIsPendingAtNow :: Property
prop_freshTodoIsPendingAtNow =
  forAll genValidTitleAndTime $ \(title, now) ->
    let todo = createTodo now title
     in getTodoStatus todo === Pending
          .&&. getTodoCreatedAt todo === now
          .&&. getTodoUpdatedAt todo === now

prop_insertAssignsSequentialIDsFromEmpty :: Property
prop_insertAssignsSequentialIDsFromEmpty =
  forAll
    genValidTodos
    ( \todos ->
        let (_finalStore, ids) = insertMany todos emptyStore
         in ids === [1 .. length todos]
    )

spec :: Spec
spec = do
  describe "parseTodoTitle" $ do
    prop "rejects an empty title string" prop_emptyTitleRejected
    prop "rejects a title string that is too long" prop_rejectTitleTooLong
    prop "undoTodoTitle roundtrips parseTodoTitle on valid titles" prop_validTitleRoundTrip

  describe "createTodo" $ do
    prop "sets status to Pending and has the time of creation as the createdAt and updatedAt timestamps" prop_freshTodoIsPendingAtNow

  describe "insertTodo" $ do
    prop "assigns sequential IDs from an empty store" prop_insertAssignsSequentialIDsFromEmpty

  describe "completeTodo" $ do
    prop "completeTodo is idempotent" prop_completingTwiceIsIdempotent
