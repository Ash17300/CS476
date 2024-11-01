package myfuzzylogic

import scala.collection.mutable
//variable types
sealed trait VarType
case object IntType extends VarType
case object StringType extends VarType
//representation of each of the features
case class ClassVar(name: String, varType: VarType)
case class Parameter(name: String, paramType: VarType)
//method def in a class
case class Method(
                   name: String,
                   parameters: List[Parameter],
                   body: List[FuzzyOperation]
                 )
//FuzzyClass def
case class FuzzyClass(
                       name: String,
                       superclass: Option[FuzzyClass] = None, //can be optional
                       variables: List[ClassVar] = List(),
                       methods: List[Method] = List(),
                       nestedClasses: List[FuzzyClass] = List() //nested class
                     )
//scope class where there may be a parent scope which can allow for heiarchy
class Scope(
             val parent: Option[Scope] = None
           ) {
  private val variables: mutable.Map[String, Any] = mutable.Map() //curr scope stores variables in mutable map
  //recursive way to get the val of a var by name
  def getVariable(name: String): Option[Any] = {
    variables.get(name).orElse(parent.flatMap(_.getVariable(name)))
  }
  //setting val of a var depending on if the var exists in the curr scope
  def setVariable(name: String, value: Any): Unit = {
    if (variables.contains(name) || parent.isEmpty) {
      variables(name) = value //set it here
    } else {
      parent.get.setVariable(name, value)//set in parent scope
    }
  }
  //new child w/ curr scope as its parent
  def createChildScope(): Scope = new Scope(Some(this))
}
//FuzzyInstance of a class
class FuzzyInstance(
                     val fuzzyClass: FuzzyClass, //instance created from fuzzyClass
                     val parentInstance: Option[FuzzyInstance] = None, //optional
                     val scope: Scope = new Scope()
                   ) {
  initializeVariables()
  //Initialize instance var
  def initializeVariables(): Unit = {
    val vars = getAllVariables(fuzzyClass)
    vars.foreach { v =>
      scope.setVariable(v.name, getDefault(v.varType))
    }
  }
  //recursive way to get all the variables from hiearchy
  def getAllVariables(cls: FuzzyClass): List[ClassVar] = {
    cls.variables ++ cls.superclass.map(getAllVariables).getOrElse(Nil)
  }
  //val of a var and can check heiarchy instance scope as well
  def getVariable(name: String): Option[Any] = {
    scope.getVariable(name).orElse(parentInstance.flatMap(_.getVariable(name)))
  }
  //set val of a var
  def setVariable(name: String, value: Any): Unit = {
    if (scope.getVariable(name).isDefined || parentInstance.isEmpty) { //if it is in the curr scope or parent scope
      scope.setVariable(name, value) //set it here
    } else {
      parentInstance.get.setVariable(name, value) //set in parent instance
    }
  }
  //get a method by name from heiarchy by searching in curr class first and then superclass
  def getMethod(name: String, cls: FuzzyClass): Option[Method] = {
    cls.methods.find(_.name == name).orElse(
      cls.superclass.flatMap(sc => getMethod(name, sc))
    )
  }
  //invoke method on instance where it returns the result from last expression/default val
  def invokeMethod(name: String, args: Map[String, Any]): Any = {
    //method is from the heiarchy
    val method = getMethod(name, fuzzyClass).getOrElse(
      throw new Exception(s"Method $name not found")
    )
    val methodScope = scope.createChildScope() //new scope
    //set the method param in the method scope
    method.parameters.foreach { param =>
      methodScope.setVariable(param.name, args.getOrElse(param.name, getDefault(param.paramType)))
    }
    val result = method.body.map(op => FuzzyEval.eval(op, Some(this), methodScope)).lastOption
    result.getOrElse(getDefault(IntType))
  }
  //nested class instance where it finds the class by name in curr class and creates a new instance
  def createNestedInstance(className: String): FuzzyInstance = {
    val nestedClass = fuzzyClass.nestedClasses.find(_.name == className).getOrElse(
      throw new Exception(s"Nested class $className not found")
    )
    new FuzzyInstance(nestedClass, Some(this), scope.createChildScope()) //this instance is the parent
  }
  //default val for var types
  def getDefault(varType: VarType): Any = varType match {
    case IntType => 0.0
    case StringType => ""
  }
}
//operations
sealed trait FuzzyOperation
case class FuzzyValue(value: Any) extends FuzzyOperation
case class FuzzyVariable(name: String) extends FuzzyOperation
case class Assign(variable: FuzzyVariable, value: FuzzyOperation) extends FuzzyOperation
case class Add(p1: FuzzyOperation, p2: FuzzyOperation) extends FuzzyOperation
case class Subtract(p1: FuzzyOperation, p2: FuzzyOperation) extends FuzzyOperation
case class Multiply(p1: FuzzyOperation, p2: FuzzyOperation) extends FuzzyOperation
case class Divide(p1: FuzzyOperation, p2: FuzzyOperation) extends FuzzyOperation
//let expression for local var
case class Let(
                assignments: List[(FuzzyVariable, FuzzyOperation)],
                body: FuzzyOperation
              ) extends FuzzyOperation
//class def
case class ClassDef(
                     name: String,
                     superclassName: Option[String], //optional
                     variables: List[ClassVar],
                     methods: List[Method],
                     nestedClasses: List[FuzzyClass] = List() //nested class
                   ) extends FuzzyOperation
case class CreateInstance(className: String) extends FuzzyOperation //new instance of a class
//invoking a method on an instance
case class InvokeMethod(
                         instanceExp: FuzzyOperation,
                         methodName: String,
                         args: List[(String, FuzzyOperation)]
                       ) extends FuzzyOperation //logic gate operation for fuzzy logic
case class TestGate(
                     gateType: String, //'and, or, not'
                     inputs: List[FuzzyOperation]
                   ) extends FuzzyOperation
object FuzzyEval {
  val classRegistry: mutable.Map[String, FuzzyClass] = mutable.Map() //global registry
  //evals fuzzy operations
  def eval(
            op: FuzzyOperation, //evaluates
            instance: Option[FuzzyInstance] = None, //optional
            scope: Scope = new Scope() //curr scope for var lookup
          ): Any = op match {
    case FuzzyValue(value) => value //const val
    //eval var by name
    case FuzzyVariable(name) =>
      scope.getVariable(name)
        .orElse(instance.flatMap(_.getVariable(name)))
        .getOrElse(throw new Exception(s"Variable $name not found"))
    //eval assignment operation
    case Assign(FuzzyVariable(name), valueOp) =>
      val value = eval(valueOp, instance, scope)
      scope.setVariable(name, value)
      value
    //eval add
    case Add(p1, p2) =>
      val v1 = eval(p1, instance, scope).asInstanceOf[Double]
      val v2 = eval(p2, instance, scope).asInstanceOf[Double]
      v1 + v2
    //eval subtract
    case Subtract(p1, p2) =>
      val v1 = eval(p1, instance, scope).asInstanceOf[Double]
      val v2 = eval(p2, instance, scope).asInstanceOf[Double]
      v1 - v2
    //eval multiply
    case Multiply(p1, p2) =>
      val v1 = eval(p1, instance, scope).asInstanceOf[Double]
      val v2 = eval(p2, instance, scope).asInstanceOf[Double]
      v1 * v2
    //eval divide
    case Divide(p1, p2) =>
      val v1 = eval(p1, instance, scope).asInstanceOf[Double]
      val v2 = eval(p2, instance, scope).asInstanceOf[Double]
      if (v2 == 0) throw new Exception("Division by zero")
      v1 / v2
    //eval let for local vars where it creates a new child scope and evaluates it in that scope
    case Let(assignments, body) =>
      val letScope = scope.createChildScope()
      //eval and assign each variable in the let scope
      assignments.foreach { case (FuzzyVariable(name), valueOp) =>
        letScope.setVariable(name, eval(valueOp, instance, letScope))
      }
      eval(body, instance, letScope)
    //class def where it creates a new fuzzy class instance and register globally
    case ClassDef(name, superclassName, variables, methods, nestedClasses) =>
      val superclass = superclassName.flatMap(classRegistry.get) //checking superclass first
      val cls = FuzzyClass(name, superclass, variables, methods, nestedClasses)
      classRegistry(name) = cls
      null
    //new class instance eval where it creates and returns a new instance of the class
    case CreateInstance(className) =>
      val cls = classRegistry.getOrElse(className, throw new Exception(s"Class $className not found"))
      new FuzzyInstance(cls)
    //invoke method on an instance
    case InvokeMethod(instanceOp, methodName, argsOps) =>
      val inst = eval(instanceOp).asInstanceOf[FuzzyInstance] //eval instance expression to get instance
      val args = argsOps.map { case (name, op) =>
        name -> eval(op, Some(inst), inst.scope)
      }.toMap
      inst.invokeMethod(methodName, args) //invoke method
    //test gate operation
    case TestGate(gateType, inputs) =>
      val evaluatedInputs = inputs.map(input => eval(input, instance, scope).asInstanceOf[Double])
      //varies based on gate type
      gateType.toUpperCase match {
        case "AND" => //returns min of inputs
          evaluatedInputs.min
        case "OR" => //return max of inputs
          evaluatedInputs.max
        case "NOT" => //return complement of input assuming there is only one input
          if (evaluatedInputs.size != 1) throw new Exception("NOT gate expects exactly one input")
          1.0 - evaluatedInputs.head
        case _ =>
          throw new Exception(s"Unknown gate type: $gateType")
      }
    case _ => throw new NotImplementedError(s"Operation $op not implemented")
  }
}
