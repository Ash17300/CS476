package myfuzzylogic

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FuzzyUnitTest extends AnyFunSuite with Matchers {
  test("TestGate operations") {
    //vars definitions
    val scope = new Scope()
    scope.setVariable("a", 0.7)
    scope.setVariable("b", 0.5)
    //AND gate test
    val andResult = FuzzyEval.eval(TestGate("AND", List(FuzzyVariable("a"), FuzzyVariable("b"))), scope = scope)
    andResult.asInstanceOf[Double] should be (0.5 +- 1e-6)
    //OR gate test
    val orResult = FuzzyEval.eval(TestGate("OR", List(FuzzyVariable("a"), FuzzyVariable("b"))), scope = scope)
    orResult.asInstanceOf[Double] should be (0.7 +- 1e-6)
    //NOT gate test
    val notResult = FuzzyEval.eval(TestGate("NOT", List(FuzzyVariable("a"))), scope = scope)
    notResult.asInstanceOf[Double] should be (0.3 +- 1e-6)
  }

  test("Assign TestGate to variable") {
    val scope = new Scope()
    scope.setVariable("x", 0.8)
    scope.setVariable("y", 0.4)
    //results from AND gate gets assigned to a var z
    FuzzyEval.eval(Assign(FuzzyVariable("z"), TestGate("AND", List(FuzzyVariable("x"), FuzzyVariable("y")))), scope = scope)
    //value of 'z' gets verified
    val zValue = scope.getVariable("z").get.asInstanceOf[Double]
    zValue should be (0.4 +- 1e-6)
  }

  test("Class with logic gate methods") {
    //a class with a method that uses TestGate
    FuzzyEval.eval(ClassDef(
      name = "LogicGateClass",
      superclassName = None,
      variables = List(ClassVar("input1", IntType), ClassVar("input2", IntType)),
      methods = List(
        Method(
          name = "computeAND",
          parameters = List(),
          body = List(
            Assign(FuzzyVariable("result"), TestGate("AND", List(FuzzyVariable("input1"), FuzzyVariable("input2")))),
            FuzzyVariable("result")
          )
        )
      )
    ))
    //instance of the class
    val instance = FuzzyEval.eval(CreateInstance("LogicGateClass")).asInstanceOf[FuzzyInstance]
    //input variables
    instance.setVariable("input1", 0.6)
    instance.setVariable("input2", 0.9)
    //compute AND method
    val result = instance.invokeMethod("computeAND", Map())
    result.asInstanceOf[Double] should be (0.6 +- 1e-6)
  }

  test("Nested classes and scopes with TestGate") {
    //outer class with a nested inner class
    FuzzyEval.eval(ClassDef(
      name = "Outer",
      superclassName = None,
      variables = List(ClassVar("x", IntType)),
      methods = List(
        Method(
          name = "setX",
          parameters = List(Parameter("value", IntType)),
          body = List(Assign(FuzzyVariable("x"), FuzzyVariable("value")))
        ),
        Method(
          name = "getX",
          parameters = List(),
          body = List(FuzzyVariable("x"))
        )
      ),
      nestedClasses = List(
        FuzzyClass(
          name = "Inner",
          variables = List(), //prevent shadowing by removing x
          methods = List(
            Method(
              name = "computeNotOuterX",
              parameters = List(),
              body = List(
                Assign(FuzzyVariable("result"), TestGate("NOT", List(FuzzyVariable("x")))),
                FuzzyVariable("result")
              )
            )
          )
        )
      )
    ))
    //instance of Outer
    val outerInstance = FuzzyEval.eval(CreateInstance("Outer")).asInstanceOf[FuzzyInstance]
    outerInstance.invokeMethod("setX", Map("value" -> 0.8))
    //instance of Inner
    val innerInstance = outerInstance.createNestedInstance("Inner")
    //invoke method in Inner that uses Outer variable 'x'
    val result = innerInstance.invokeMethod("computeNotOuterX", Map())
    result.asInstanceOf[Double] should be (0.2 +- 1e-6)
  }
  test("Method overriding and dynamic dispatch") {
    //base class w/ a method
    FuzzyEval.eval(ClassDef(
      name = "Base",
      superclassName = None,
      variables = List(),
      methods = List(
        Method(
          name = "greet",
          parameters = List(),
          body = List(FuzzyValue("Hello from Base"))
        )
      )
    ))
    //derived class that extends Base and overrides greet
    FuzzyEval.eval(ClassDef(
      name = "Derived",
      superclassName = Some("Base"),
      variables = List(),
      methods = List(
        Method(
          name = "greet",
          parameters = List(),
          body = List(FuzzyValue("Hello from Derived"))
        )
      )
    ))
    //instance of derived
    val derivedInstance = FuzzyEval.eval(CreateInstance("Derived")).asInstanceOf[FuzzyInstance]
    //invoke the greet method
    val greeting = derivedInstance.invokeMethod("greet", Map())
    greeting shouldEqual "Hello from Derived"
  }
}
