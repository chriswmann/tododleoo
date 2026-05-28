{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DomainSpec (spec) where

import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Time (UTCTime (..))
import Domain (TitleError (..), TodoStatus (..), TodoTitle (unTodoTitle), completeTodo, createTodo, emptyStore, getTodo, getTodoCreatedAt, getTodoStatus, getTodoUpdatedAt, insertTodo, parseTodoTitle)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, arbitraryUnicodeChar, chooseInt, elements, forAll, suchThat, vectorOf, (===))
import Test.QuickCheck.Instances ()

-- Define these Arbitrary instances here to keep QuickCheck dependencies out of the production module
instance Arbitrary TodoStatus where
  arbitrary = elements [minBound .. maxBound]

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

prop_emptyTitleRejected :: Property
prop_emptyTitleRejected =
  forAll (elements ["", " ", "   ", "\t\n"]) $ \blank -> parseTodoTitle blank === Left EmptyTitle

prop_validTitleRoundTrip :: Property
prop_validTitleRoundTrip = forAll genValidTitleText $ \t -> fmap unTodoTitle (parseTodoTitle t) === Right t

prop_completingTwiceIsIdempotent :: UTCTime -> Property
prop_completingTwiceIsIdempotent now = createTodo now (parseTodoTitle)

spec :: Spec
spec = do
  describe "parseTodoTitle" $ do
    prop "rejects an empty title string" prop_emptyTitleRejected
    prop "undoTodoTitle roundtrips parseTodoTitle on valid titles" prop_validTitleRoundTrip
