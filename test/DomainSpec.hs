module DomainSpec (spec) where

import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian)
import Domain (createTodo, title)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "createTodo" $ do
    it "uses the title that was passed in" $ do
      let now = UTCTime (fromGregorian 2026 05 15) 0
      let todo = createTodo now (T.pack "This title")
      title todo `shouldBe` T.pack "This title"
