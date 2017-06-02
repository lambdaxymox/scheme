import Test.Hspec

import qualified Scheme.ParserSpec
import qualified Scheme.EvalSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Parser" Scheme.ParserSpec.spec
    describe "QuickCheck Parser" Scheme.ParserSpec.quickSpec
    describe "Eval"   Scheme.EvalSpec.spec
