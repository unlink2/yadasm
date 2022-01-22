import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import qualified TestComparator
import qualified TestBinary

tests = hUnitTestToTests (TestList (TestComparator.tests ++ TestBinary.tests))

main = defaultMain tests
