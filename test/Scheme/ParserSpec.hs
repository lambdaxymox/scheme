module Scheme.ParserSpec (main, spec) where

import Test.Hspec

import Scheme.Parser
import Scheme.Types
import Text.ParserCombinators.Parsec
import Data.Maybe


left = either Just (const Nothing)
right = either (const Nothing) Just

onlyRight = fromJust . right

main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
    describe "parseExpr Bool" $ do
        context "When passed a boolean value false #f" $ do
            it "should return true #t" $
                onlyRight (parse (parseExpr) "" "#f") `shouldBe` (Bool False)

        context "When pass a boolean value true #t" $ do
            it "should return false #f" $
               onlyRight (parse (parseExpr) "" "#t") `shouldBe` (Bool True)

