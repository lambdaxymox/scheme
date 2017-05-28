module Scheme.ParserSpec (main, spec) where

import Test.Hspec
import Scheme.Parser
import Scheme.Types
import Text.ParserCombinators.Parsec
import Data.Maybe


right :: Either e a -> Maybe a
right = either (const Nothing) Just

onlyRight :: Either e a -> a
onlyRight = fromJust . right

quotedVal :: LispVal -> LispVal 
quotedVal val = List [Atom "quote", val]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
    describe "parseExpr Bool" $ do
        context "When passed a boolean value false #f" $ do
            it "should return true #f" $
                let possiblyFalse = onlyRight $ parse parseExpr "" "#f"
                in  possiblyFalse `shouldBe` Bool False

        context "When passed a boolean value true #t" $ do
            it "should return false #t" $
                let possiblyTrue = onlyRight $ parse parseExpr "" "#t"
                in  possiblyTrue `shouldBe` Bool True

        context "When passed a boolean value as a literal \'#t" $ do
            it "should evaluate as a constant value #t" $
                let possiblyTrue = onlyRight $ parse parseExpr "" "'#t"
                    quotedTrue   = quotedVal (Bool True)
                in  possiblyTrue `shouldBe` quotedTrue

        context "When passed a boolean value as a literal \'#f" $ do
            it "should evaluate as a constant value #f" $
                let possiblyFalse = onlyRight $ parse parseExpr "" "'#f"
                    quotedFalse   = quotedVal (Bool False)
                in  possiblyFalse `shouldBe` quotedFalse

    describe "parseExpr Number" $ do
        context "When passed a decimal number" $ do
            it "should parse to a correct integer" $
                let possibleInteger = onlyRight $ parse parseExpr "" "123456"
                in  possibleInteger `shouldBe` Number (123456 :: Integer)