import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import qualified Yadasm.TestComparator
import qualified Yadasm.TestBinary
import qualified Yadasm.TestNode

tests = hUnitTestToTests
  (TestList
     (Yadasm.TestComparator.tests
      ++ Yadasm.TestBinary.tests
      ++ Yadasm.TestNode.tests))

main = defaultMain tests
