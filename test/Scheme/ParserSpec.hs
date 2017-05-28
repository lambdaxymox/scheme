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

parseValue :: String -> LispVal
parseValue st = onlyRight $ parse parseExpr "" st

main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
    describe "parseExpr Bool" $ do
        context "When passed a boolean value false #f" $ do
            it "should return true #f" $
                let possiblyFalse = parseValue "#f"
                in  possiblyFalse `shouldBe` Bool False

        context "When passed a boolean value true #t" $ do
            it "should return false #t" $
                let possiblyTrue = parseValue "#t"
                in  possiblyTrue `shouldBe` Bool True

        context "When passed a boolean value as a literal \'#t" $ do
            it "should evaluate as a constant value #t" $
                let possiblyTrue = parseValue "'#t"
                    quotedTrue   = quotedVal (Bool True)
                in  possiblyTrue `shouldBe` quotedTrue

        context "When passed a boolean value as a literal \'#f" $ do
            it "should evaluate as a constant value #f" $
                let possiblyFalse = parseValue "'#f"
                    quotedFalse   = quotedVal (Bool False)
                in  possiblyFalse `shouldBe` quotedFalse

    describe "parseExpr Number" $ do
        context "When passed a decimal number" $ do
            it "should parse to a correct decimal integer" $
                let possibleInteger = parseValue "123456"
                in  possibleInteger `shouldBe` Number (123456 :: Integer)

        context "When passed a number with a specified base" $ do
            it "should parse to a correct binary number" $
                let possibleBinaryNumber = parseValue "#b011110001001000000" -- 123456
                in  possibleBinaryNumber `shouldBe` Number 123456

            it "should parse to a correct octal number" $
                let possibleOctalNumber = parseValue "#o0361100" --123456
                in  possibleOctalNumber `shouldBe` Number 0o361100

            it "should parse to a correct hexadecimal number" $
                let possibleHexNumber = parseValue "#x1E240" --123456
                in  possibleHexNumber `shouldBe` Number 0x1E240

            it "should parse to a correct deimal number" $
                let possibleDecNumber = parseValue "#d123456"
                in  possibleDecNumber `shouldBe` Number 123456