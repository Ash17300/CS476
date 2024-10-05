import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import FuzzyEval._
import scala.collection.mutable

class FuzzyUnitTest extends AnyFlatSpec with Matchers {

  given env: mutable.Map[String, Double] = mutable.Map()

  "Fuzzy Logic Evaluator" should "evaluate fuzzy union" in {
    eval(FuzzyOperation.Assign(FuzzyOperation.FuzzyVariable("A"), FuzzyOperation.FuzzyValue(0.7)))
    eval(FuzzyOperation.FuzzyUnion(FuzzyOperation.FuzzyVariable("A"), FuzzyOperation.FuzzyValue(0.5))) shouldEqual 0.7 +- 0.0001
  }

  it should "evaluate fuzzy complement" in {
    eval(FuzzyOperation.FuzzyComplement(FuzzyOperation.FuzzyVariable("A"))) shouldEqual 0.3 +- 0.0001
  }

  it should "evaluate fuzzy intersection" in {
    eval(FuzzyOperation.Assign(FuzzyOperation.FuzzyVariable("B"), FuzzyOperation.FuzzyValue(0.4)))
    eval(FuzzyOperation.FuzzyIntersection(FuzzyOperation.FuzzyVariable("A"), FuzzyOperation.FuzzyVariable("B"))) shouldEqual 0.4 +- 0.0001
  }
}
