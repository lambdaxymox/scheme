import Test.Hspec

import qualified Scheme.ParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Parser" ParserSpec.spec
