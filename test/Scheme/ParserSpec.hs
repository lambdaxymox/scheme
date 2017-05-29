module Scheme.ParserSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Scheme.Parser
import Scheme.Types
import Text.ParserCombinators.Parsec
import Data.Maybe
import Numeric


right :: Either e a -> Maybe a
right = either (const Nothing) Just

onlyRight :: Either e a -> a
onlyRight = fromJust . right

quotedVal :: LispVal -> LispVal 
quotedVal val = List [Atom "quote", val]

parseValue :: String -> LispVal
parseValue st = onlyRight $ parse parseExpr "" st
{-
parseEither :: String -> Either e LispVal
parseEither st = parse parseExpr "" st
-}
main :: IO ()
main = hspec $ do
    describe "Unit Spec" spec
    describe "QuickCheck Spec" quickSpec

spec :: Spec
spec = do 
    describe "parseExpr" $ do
        context "Bool" $ do
            it "should return false with input \"#f\"" $
                let possiblyFalse = parseValue "#f"
                in  possiblyFalse `shouldBe` Bool False

            it "should return true with input \"#t\"" $
                let possiblyTrue = parseValue "#t"
                in  possiblyTrue `shouldBe` Bool True

            it "When passed a boolean literal \'#t" $
                let possiblyTrue = parseValue "'#t"
                    quotedTrue   = quotedVal (Bool True)
                in  possiblyTrue `shouldBe` quotedTrue

            it "When passed a boolean literal \'#f" $
                let possiblyFalse = parseValue "'#f"
                    quotedFalse   = quotedVal (Bool False)
                in  possiblyFalse `shouldBe` quotedFalse

    describe "parseExpr Number" $ do
        context "When passed a decimal number" $ do
            it "should parse to a correct decimal integer" $
                let possibleInteger = parseValue "123456"
                in  possibleInteger `shouldBe` Number 123456

        context "When passed a number with a specified base" $ do
            it "should correctly parse a binary number" $
                let possibleBinaryNumber = parseValue "#b011110001001000000" -- 123456
                in  possibleBinaryNumber `shouldBe` Number 123456

            it "should correctly parse an octal number" $
                let possibleOctalNumber = parseValue "#o0361100" --123456
                in  possibleOctalNumber `shouldBe` Number 0o361100

            it "should correctly parse a hexadecimal number" $
                let possibleHexNumber = parseValue "#x1E240" --123456
                in  possibleHexNumber `shouldBe` Number 0x1E240

            it "should correctly parse a decimal number" $
                let possibleDecNumber = parseValue "#d123456"
                in  possibleDecNumber `shouldBe` Number 123456
            
    describe "parseExpr String" $ do
        context "When passed an ordinary string" $ do
            it "should parse the string" $
                let st = parseValue "\"This is a string.\""
                in  st `shouldBe` String "This is a string."

        context "When passed a string with escaped quotes" $ do
            it "should include the quotes in the resulting string" $
                let st = parseValue "\"This is a \\\"quoted\\\" string.\""
                in  st `shouldBe` String "This is a \"quoted\" string."

        context "When pass a string with one escaped quote" $ do
            it "should parse a the string" $
                let st = parseValue "\"This is a \\\"half-quoted string.\""
                in  st `shouldBe` String "This is a \"half-quoted string."


bases :: Gen LispBase
bases = elements [Base2, Base8, Base10, Base16]

showDec :: Integer -> String
showDec = show

showBin :: Integer -> String
showBin num = showIntAtBase 2 conv num ""
    where
        conv :: Int -> Char
        conv d = "01" !! d

data LispBase = Base2 | Base8 | Base10 | Base16

instance Show LispBase where
    show Base2  = "#b"
    show Base8  = "#o"
    show Base10 = "#d"
    show Base16 = "#x"

data BasedNumber = MkBasedNumber { lispBase :: LispBase, lispNum :: Integer }

instance Show BasedNumber where
    show (MkBasedNumber Base2 num)  = show Base2  ++ showBin num
    show (MkBasedNumber Base8 num)  = show Base8  ++ showOct num ""
    show (MkBasedNumber Base10 num) = show Base10 ++ showDec num
    show (MkBasedNumber Base16 num) = show Base16 ++ showHex num ""

basedNumber :: Gen BasedNumber
basedNumber = do
    base <- bases
    num  <- arbitrary
    return $ MkBasedNumber base num

instance Arbitrary BasedNumber where
    arbitrary = basedNumber


quickSpec :: Spec
quickSpec = do
    describe "parseExpr Number" $ do
        context "When passed an integer in the correct base" $ do
            it "should parse the string to the correct integer" $ 
                property $ forAll basedNumber $ \bn -> 
                    parseValue (show $ lispBase bn) === Number (lispNum bn)
