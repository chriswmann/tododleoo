module DamerauLevenshteinSpec (spec) where

import DamerauLevenshtein (levenshtein)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, arbitraryUnicodeChar, chooseInt, forAll, vectorOf, (.&&.), (===))

genWord :: Gen String
genWord = chooseInt (0, 64) >>= \len -> vectorOf len arbitraryUnicodeChar

prop_identicalWordDistanceIsZero :: Property
prop_identicalWordDistanceIsZero = forAll genWord $ \a -> levenshtein a a === 0

prop_distanceIsLengthWhenOtherWordEmpty :: Property
prop_distanceIsLengthWhenOtherWordEmpty = forAll genWord $ \a -> levenshtein a "" === length a .&&. levenshtein "" a === length a

prop_distanceIsSymmetric :: Property
prop_distanceIsSymmetric =
  forAll ((,) <$> genWord <*> genWord) $ \(a, b) -> levenshtein a b === levenshtein b a

prop_triangleInequality :: Property
prop_triangleInequality = forAll ((,,) <$> genWord <*> genWord <*> genWord) $ \(a, b, c) -> levenshtein a c <= (levenshtein a b + levenshtein b c)

spec :: Spec
spec = do
  describe "levenshtein" $ do
    it "two empty words have a distance of zero" $ levenshtein "" "" `shouldBe` 0
    it "Damerau transposition has a distance of 1" $ levenshtein "ab" "ba" `shouldBe` 1
    it "levenshtein kitten sitting is 3" $ levenshtein "kitten" "sitting" `shouldBe` 3
    it "levenshtein remove remvoe is 1" $ levenshtein "remove" "remvoe" `shouldBe` 1
    it "levenshtein complete comlete is 1" $ levenshtein "complete" "comlete" `shouldBe` 1
    prop "identical words always have zero distance" prop_identicalWordDistanceIsZero
    prop "distance is always the length of the other word, when one word is 'empty'" prop_distanceIsLengthWhenOtherWordEmpty
    prop "distance is symmetric" prop_distanceIsSymmetric
    prop "distance of a to c is less than or equal to (a to b plus b to c)" prop_triangleInequality
