package myfuzzylogic

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FuzzyUnitTest extends AnyFunSuite with Matchers {

  test("Partial evaluation of arithmetic expression with undefined variable") {
    val expr = Multiply(FuzzyValue(3), Multiply(Add(FuzzyValue(5), FuzzyValue(1)), FuzzyVariable("var")))
    val result = FuzzyEval.partialEval(expr)
    // Expected: Multiply(FuzzyValue(18.0), FuzzyVariable("var"))
    result shouldEqual Multiply(FuzzyValue(18.0), FuzzyVariable("var"))
  }

  test("Partial evaluation with operator associativity") {
    val expr = Multiply(FuzzyValue(3), Multiply(FuzzyValue(5), FuzzyVariable("var")))
    val result = FuzzyEval.partialEval(expr)
    // Expected: Multiply(FuzzyValue(15.0), FuzzyVariable("var"))
    result shouldEqual Multiply(FuzzyValue(15.0), FuzzyVariable("var"))
  }

  test("Partial evaluation of conditional construct with undefined variables") {
    val condition = GreaterEqual(Multiply(FuzzyValue(15), FuzzyVariable("var")), Add(FuzzyValue(2), FuzzyVariable("var1")))
    val thenBranch = Assign(FuzzyVariable("somevar"), Add(FuzzyVariable("var"), FuzzyValue(3)))
    val elseBranch = FuzzyValue("Else branch executed")

    val expr = IfTrue(condition, thenBranch, elseBranch)
    val result = FuzzyEval.partialEval(expr)

    // Since variables are undefined, expect a partially evaluated IfTrue
    result shouldBe an [IfTrue]
  }

  test("Partial evaluation of methods with dynamic dispatch") {
    // Define classes with methods that override each other
    FuzzyEval.partialEval(ClassDef(
      name = "Base",
      superclassName = None,
      variables = List(),
      methods = List(
        Method(
          name = "compute",
          parameters = List(),
          body = List(Add(FuzzyVariable("x"), FuzzyValue(1)))
        )
      )
    ))

    FuzzyEval.partialEval(ClassDef(
      name = "Derived",
      superclassName = Some("Base"),
      variables = List(),
      methods = List(
        Method(
          name = "compute",
          parameters = List(),
          body = List(Add(FuzzyVariable("x"), FuzzyValue(2)))
        )
      )
    ))

    // Create an instance of Derived
    val instance = FuzzyEval.partialEval(CreateInstance("Derived")).asInstanceOf[FuzzyInstance]
    instance.setVariable("x", FuzzyVariable("x")) // x is undefined

    // Invoke compute method
    val result = instance.invokeMethod("compute", Map())

    // Should return partially evaluated methods from both Base and Derived
    result shouldBe a [List[_]]
    result.asInstanceOf[List[PartiallyEvaluatedMethod]].length shouldEqual 2
  }

  test("Partial evaluation of TestGate with undefined variables") {
    val expr = TestGate("AND", List(FuzzyVariable("a"), FuzzyValue(0.5)))
    val result = FuzzyEval.partialEval(expr)
    // Since 'a' is undefined, expect a partially evaluated TestGate
    result shouldBe a [TestGate]
  }

  test("Full evaluation when variables are defined") {
    val scope = new Scope()
    scope.setVariable("a", 0.7)
    scope.setVariable("b", 0.5)

    val expr = Add(FuzzyVariable("a"), FuzzyVariable("b"))
    val result = FuzzyEval.eval(expr, scope = scope)
    result.asInstanceOf[Double] should be (1.2 +- 1e-6)
  }

  test("Conditional evaluation when condition is true") {
    val scope = new Scope()
    scope.setVariable("var", 2.0)
    scope.setVariable("var1", 1.0)

    val condition = GreaterEqual(Multiply(FuzzyValue(15), FuzzyVariable("var")), Add(FuzzyValue(2), FuzzyVariable("var1")))
    val thenBranch = Assign(FuzzyVariable("somevar"), Add(FuzzyVariable("var"), FuzzyValue(3)))
    val elseBranch = FuzzyValue("Else branch executed")

    val expr = IfTrue(condition, thenBranch, elseBranch)
    val result = FuzzyEval.eval(expr, scope = scope)

    // 'somevar' should be assigned 5.0
    scope.getVariable("somevar").get.asInstanceOf[Double] should be (5.0 +- 1e-6)
  }

  test("Conditional evaluation when condition is false") {
    val scope = new Scope()
    scope.setVariable("var", 0.0)
    scope.setVariable("var1", 10.0)

    val condition = GreaterEqual(Multiply(FuzzyValue(15), FuzzyVariable("var")), Add(FuzzyValue(2), FuzzyVariable("var1")))
    val thenBranch = Assign(FuzzyVariable("somevar"), Add(FuzzyVariable("var"), FuzzyValue(3)))
    val elseBranch = Assign(FuzzyVariable("somevar"), FuzzyValue(42.0))

    val expr = IfTrue(condition, thenBranch, elseBranch)
    val result = FuzzyEval.eval(expr, scope = scope)

    // 'somevar' should be assigned 42.0
    scope.getVariable("somevar").get.asInstanceOf[Double] should be (42.0 +- 1e-6)
  }
}
