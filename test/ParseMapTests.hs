module ParseMapTests
  ( parseMapTests
  ) where

import Test.Hspec
import ParseMap

parseMapTests :: IO()
parseMapTests = hspec $
  describe "Parser functions" $ do
    it "parses small map correctly" $
      parseMap "ooo/o1o/ooo" `shouldBe` (["ooo",
                                          "o o",
                                          "ooo"] :: [String])
    it "parses larger map correctly" $
      parseMap "oooooooooooo/oB9o/o10o/o10o/o10o/o10o/o10o/o10o/o10o/o10o/o9Ro/oooooooooooo"
               `shouldBe` (["oooooooooooo",
                            "oB         o",
                            "o          o",
                            "o          o",
                            "o          o",
                            "o          o",
                            "o          o",
                            "o          o",
                            "o          o",
                            "o          o",
                            "o         Ro",
                            "oooooooooooo"] :: [String])
