import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import qualified Yadasm.TestComparator
import qualified Yadasm.TestBinary
import qualified Yadasm.TestNode
import qualified Yadasm.TestParser
import qualified Yadasm.TestContext
import qualified Yadasm.TestLine
import qualified Yadasm.Archs.TestArch6502
import qualified Yadasm.Archs.TestArch65C02
import qualified Yadasm.Archs.TestArch65C816

tests = hUnitTestToTests
  (TestList
     (Yadasm.TestComparator.tests
      ++ Yadasm.TestBinary.tests
      ++ Yadasm.TestNode.tests
      ++ Yadasm.TestParser.tests
      ++ Yadasm.TestContext.tests
      ++ Yadasm.TestLine.tests
      ++ Yadasm.Archs.TestArch6502.tests
      ++ Yadasm.Archs.TestArch65C02.tests
      ++ Yadasm.Archs.TestArch65C816.tests))

main = defaultMain tests
