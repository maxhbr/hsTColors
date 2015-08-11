module System.HsTColorsSpec
       where

import SpecHelper
import System.HsTColors

spec :: Spec
spec = do
  describe "HsTcolors" $ do
    it "ansiCode returns the correct code" $ do
      property $ \s -> (colorString (ANSICode (toANSICode ANSIRed))) s == redString (s :: String)
    it "uncolor returns old string" $ do
      property $ \s -> (uncolor . redString) s == (s :: String)
    it "uncolLength gets length of String" $ do
      property $ \s -> uncolLength s == (length . uncolor) (s :: String)

main :: IO ()
main = hspec $ spec
