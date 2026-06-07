module CommandSpec (spec) where

import Command (CommandError (..), parseCommand)
import Data.List (uncons)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Gen, Property, counterexample, (===))

newtype NonKeyword = NonKeyword String deriving (Show, Eq)

mk :: String -> NonKeyword
mk s = NonKeyword ('X' : s)

instance Arbitrary NonKeyword where
  arbitrary = mk <$> (arbitrary :: Gen String)
  shrink (NonKeyword s) =
    [mk s' | s' <- shrink base]
    where
      base = drop 1 s

prop_nonKeywordParsesAsUnknownVerbError :: NonKeyword -> Property
prop_nonKeywordParsesAsUnknownVerbError (NonKeyword s) = case parseCommand s of
  Left (UnknownVerb firstWord _) -> firstWord === case uncons (words s) of
    Just (inputVerb, _) -> inputVerb
    Nothing -> error "cannot get an empty verb here as `NonKeyword` always starts with 'X'"
  other -> counterexample (show other) False

spec :: Spec
spec = do
  describe "parseCommand" $ do
    prop "parses a non-keyword verb as `Left UnknownVerb`" prop_nonKeywordParsesAsUnknownVerbError
