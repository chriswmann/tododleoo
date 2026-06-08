module CommandSpec (spec) where

import Command (Command (..), CommandError (..), MetaCommand (..), StoreCommand (..), parseCommand)
import Data.List (uncons)
import Domain.Internal (TodoId (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Gen, Property, counterexample, elements, listOf1, oneof, (===))
import Test.QuickCheck.Instances ()

newtype NonKeyword = NonKeyword String deriving (Show, Eq)

mk :: String -> NonKeyword
mk s = NonKeyword ('X' : s)

instance Arbitrary NonKeyword where
  arbitrary = mk <$> (arbitrary :: Gen String)
  shrink (NonKeyword s) =
    [mk s' | s' <- shrink base]
    where
      base = drop 1 s

data CommandCase = CommandCase String Command deriving (Show, Eq)

instance Arbitrary CommandCase where
  arbitrary =
    oneof
      [ pure (CommandCase "list" (Store List)),
        pure (CommandCase "help" (Meta Help)),
        pure (CommandCase "quit" (Meta Quit)),
        pure (CommandCase "exit" (Meta Quit)),
        genInt "done" Complete,
        genInt "remove" Delete,
        genInt "view" View,
        genAdd
      ]

genInt :: String -> (TodoId -> StoreCommand) -> Gen CommandCase
genInt verb constructor = do
  n <- arbitrary
  let input = verb ++ " " ++ show n
      expected = Store (constructor (TodoId n))
  pure (CommandCase input expected)

genWord :: Gen String
genWord = listOf1 (elements alphabet)
  where
    alphabet = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

genAdd :: Gen CommandCase
genAdd = do
  ws <- listOf1 genWord
  let input = "add " ++ unwords ws
      expected = Store (Create (unwords ws))
  pure (CommandCase input expected)

prop_nonKeywordParsesAsUnknownVerbError :: NonKeyword -> Property
prop_nonKeywordParsesAsUnknownVerbError (NonKeyword s) = case parseCommand s of
  Left (UnknownVerb firstWord _) ->
    firstWord === case uncons (words s) of
      Just (inputVerb, _) -> inputVerb
      Nothing -> error "cannot get an empty verb here as `NonKeyword` always starts with 'X'"
  other -> counterexample (show other) False

prop_knownCommandParsesToExpected :: CommandCase -> Property
prop_knownCommandParsesToExpected (CommandCase input expected) =
  parseCommand input === Right expected

spec :: Spec
spec = do
  describe "parseCommand" $ do
    it "empty input returns `NoCommand` error" $ parseCommand "" `shouldBe` Left NoCommand
    it "rejects a numeric verb with no argument" $ parseCommand "done" `shouldBe` Left (BadArguments [])
    it "rejects a non-numeric argument" $ parseCommand "done abc" `shouldBe` Left (BadArguments ["abc"])
    it "rejects a negative ID" $ parseCommand "done -1" `shouldBe` Left (BadArguments ["-1"])
    prop "parses a non-keyword verb as `Left UnknownVerb`" prop_nonKeywordParsesAsUnknownVerbError
    prop "parses known commands to the expected Command" prop_knownCommandParsesToExpected

  describe "suggest" $ do
    it "offers a suggestion for a near–miss verb" $
      parseCommand "halp" `shouldBe` Left (UnknownVerb "halp" (Just "help"))
