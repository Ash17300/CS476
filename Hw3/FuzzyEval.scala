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
//scope class where there may be a parent scope which can allow for hierarchy
class Scope(val parent: Option[Scope] = None) {
  private val variables: mutable.Map[String, Any] = mutable.Map() //curr scope stores variables in mutable map
  //recursive way to get the value of a var by name
  def getVariable(name: String): Option[Any] = {
    variables.get(name).orElse(parent.flatMap(_.getVariable(name)))
  }
  //setting val of a var depending on if the var exists in the curr scope
  def setVariable(name: String, value: Any): Unit = {
    if (variables.contains(name) || parent.isEmpty) {
      variables(name) = value
    } else {
      parent.get.setVariable(name, value) //set in parent scope
    }
  }
  //new child with curr scope as its parent
  def createChildScope(): Scope = new Scope(Some(this))
}
//FuzzyInstance of a class
class FuzzyInstance(
                     val fuzzyClass: FuzzyClass,
                     val parentInstance: Option[FuzzyInstance] = None, //optional
                     val scope: Scope = new Scope()
                   ) {
  initializeVariables()
  //initialize instance vars
  def initializeVariables(): Unit = {
    val vars = getAllVariables(fuzzyClass)
    vars.foreach { v =>
      scope.setVariable(v.name, getDefault(v.varType))
    }
  }
  //recursive way to get all the variables from the hierarchy
  def getAllVariables(cls: FuzzyClass): List[ClassVar] = {
    cls.variables ++ cls.superclass.map(getAllVariables).getOrElse(Nil)
  }
  //val of a var and can check hierarchy instance scope as well
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
  //get a method by name from hierarchy by searching in curr class first and then superclass
  def getMethods(name: String, cls: FuzzyClass): List[Method] = {
    val methodsInClass = cls.methods.filter(_.name == name)
    val methodsInSuper = cls.superclass.map(sc => getMethods(name, sc)).getOrElse(Nil)
    methodsInClass ++ methodsInSuper
  }
  //invoke method on instance where it returns the result from last expression/default val
  def invokeMethod(name: String, args: Map[String, Any]): Any = {
    val methods = getMethods(name, fuzzyClass)
    if (methods.isEmpty) throw new Exception(s"Method $name not found")
    val partiallyEvaluatedMethods = methods.map { method =>
      partialEvalMethod(method, args)
    }
    partiallyEvaluatedMethods
  }
  // Partially evaluate a method
  def partialEvalMethod(method: Method, args: Map[String, Any]): PartiallyEvaluatedMethod = {
    val methodScope = scope.createChildScope()
    //setting method param in the method scope
    method.parameters.foreach { param =>
      methodScope.setVariable(param.name, args.getOrElse(param.name, FuzzyVariable(param.name)))
    }
    //method body gets partially evaluated
    val partiallyEvaluatedBody = method.body.map(op => FuzzyEval.partialEval(op, Some(this), methodScope))
    PartiallyEvaluatedMethod(method.name, method.parameters, partiallyEvaluatedBody)
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
    case IntType    => 0.0
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
case class InvokeMethod(
                         instanceExp: FuzzyOperation,
                         methodName: String,
                         args: List[(String, FuzzyOperation)]
                       ) extends FuzzyOperation //logic gate operation for fuzzy logic
case class TestGate(
                     gateType: String, //'and, or, not'
                     inputs: List[FuzzyOperation]
                   ) extends FuzzyOperation
case class IfTrue(condition: FuzzyOperation, thenBranch: FuzzyOperation, elseBranch: FuzzyOperation) extends FuzzyOperation
sealed trait ComparisonOperation extends FuzzyOperation
case class GreaterEqual(left: FuzzyOperation, right: FuzzyOperation) extends ComparisonOperation
//partially evaluated method
case class PartiallyEvaluatedMethod(
                                     name: String,
                                     parameters: List[Parameter],
                                     body: List[Any]
                                   )
object FuzzyEval {
  val classRegistry: mutable.Map[String, FuzzyClass] = mutable.Map() //global registry
  //evaluates fuzzy operations
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
      val v1 = eval(p1, instance, scope)
      val v2 = eval(p2, instance, scope)
      (v1, v2) match {
        case (a: Number, b: Number) => a.doubleValue() + b.doubleValue()
        case _ => throw new Exception("Invalid operands for addition")
      }
    //eval subtract
    case Subtract(p1, p2) =>
      val v1 = eval(p1, instance, scope)
      val v2 = eval(p2, instance, scope)
      (v1, v2) match {
        case (a: Number, b: Number) => a.doubleValue() - b.doubleValue()
        case _ => throw new Exception("Invalid operands for subtraction")
      }
    //eval multiply
    case Multiply(p1, p2) =>
      val v1 = eval(p1, instance, scope)
      val v2 = eval(p2, instance, scope)
      (v1, v2) match {
        case (a: Number, b: Number) => a.doubleValue() * b.doubleValue()
        case _ => throw new Exception("Invalid operands for multiplication")
      }
    //eval divide
    case Divide(p1, p2) =>
      val v1 = eval(p1, instance, scope)
      val v2 = eval(p2, instance, scope)
      (v1, v2) match {
        case (a: Number, b: Number) =>
          if (b.doubleValue() == 0) throw new Exception("Division by zero")
          else a.doubleValue() / b.doubleValue()
        case _ => throw new Exception("Invalid operands for division")
      }
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
      val evaluatedInputs = inputs.map(input => eval(input, instance, scope))
      //varies based on gate type
      if (evaluatedInputs.forall(_.isInstanceOf[Number])) {
        val inputValues = evaluatedInputs.map(_.asInstanceOf[Number].doubleValue())
        gateType.toUpperCase match {
          case "AND" => inputValues.min //returns min of inputs
          case "OR"  => inputValues.max //return max of inputs
          case "NOT" => //return complement of input assuming there is only one input
            if (inputValues.size != 1) throw new Exception("NOT gate expects exactly one input")
            1.0 - inputValues.head
          case _ => throw new Exception(s"Unknown gate type: $gateType")
        }
      } else {
        throw new Exception("Invalid inputs for TestGate")
      }
    //ifTrue evaluation
    case IfTrue(condition, thenBranch, elseBranch) =>
      val condValue = eval(condition, instance, scope)
      condValue match {
        case true  => eval(thenBranch, instance, scope)
        case false => eval(elseBranch, instance, scope)
        case _     => throw new Exception("Condition did not evaluate to a boolean value")
      }
    //comparision operation
    case GreaterEqual(left, right) =>
      val v1 = eval(left, instance, scope)
      val v2 = eval(right, instance, scope)
      (v1, v2) match {
        case (a: Number, b: Number) => a.doubleValue() >= b.doubleValue()
        case _ => throw new Exception("Invalid operands for comparison")
      }
    case _ => throw new NotImplementedError(s"Operation $op not implemented")
  }
  // Partial evaluation function
  def partialEval(
                   op: FuzzyOperation,
                   instance: Option[FuzzyInstance] = None,
                   scope: Scope = new Scope()
                 ): Any = op match {
    case FuzzyValue(value) => FuzzyValue(value) //return the value as is
    case FuzzyVariable(name) =>
      //getting the vars value from scope or instance and if not found then return the var itself
      scope.getVariable(name)
        .orElse(instance.flatMap(_.getVariable(name)))
        .getOrElse(FuzzyVariable(name))
    case Assign(FuzzyVariable(name), valueOp) =>
      val value = partialEval(valueOp, instance, scope) //partially eval the val and assign it to the var in scope
      scope.setVariable(name, value)
      Assign(FuzzyVariable(name), value.asInstanceOf[FuzzyOperation]) //returning w/ partially evaluated val
    //partial eval addition in simplifed
    case Add(p1, p2) =>
      val v1 = partialEval(p1, instance, scope)
      val v2 = partialEval(p2, instance, scope)
      simplifyAdd(v1, v2)
    //partial eval subtraction in simplifed
    case Subtract(p1, p2) =>
      val v1 = partialEval(p1, instance, scope)
      val v2 = partialEval(p2, instance, scope)
      simplifySubtract(v1, v2)
    //partial eval multiplication in simplifed
    case Multiply(p1, p2) =>
      val v1 = partialEval(p1, instance, scope)
      val v2 = partialEval(p2, instance, scope)
      simplifyMultiply(v1, v2)
    //partial eval division in simplifed
    case Divide(p1, p2) =>
      val v1 = partialEval(p1, instance, scope)
      val v2 = partialEval(p2, instance, scope)
      simplifyDivide(v1, v2)
    //let partial
    case Let(assignments, body) =>
      val letScope = scope.createChildScope() //new child scope
      //partial eval and assign var in let scope
      val partialAssignments = assignments.map { case (FuzzyVariable(name), valueOp) =>
        val value = partialEval(valueOp, instance, letScope)
        letScope.setVariable(name, value)
        (FuzzyVariable(name), value.asInstanceOf[FuzzyOperation])
      }
      //partially eval body in let scope
      val partialBody = partialEval(body, instance, letScope)
      Let(partialAssignments, partialBody.asInstanceOf[FuzzyOperation]) //returns the partially eval let expression
    case ClassDef(name, superclassName, variables, methods, nestedClasses) =>
      val superclass = superclassName.flatMap(classRegistry.get) //new class which gets added to classs registry
      val cls = FuzzyClass(name, superclass, variables, methods, nestedClasses)
      classRegistry(name) = cls
      null
    //new instance of the specified class
    case CreateInstance(className) =>
      val cls = classRegistry.getOrElse(className, throw new Exception(s"Class $className not found"))
      new FuzzyInstance(cls)
    //invoke method on instance w/ partial eval args
    case InvokeMethod(instanceOp, methodName, argsOps) =>
      val inst = partialEval(instanceOp).asInstanceOf[FuzzyInstance] //partial eval experssion
      val args = argsOps.map { case (name, op) => //partial eval args
        name -> partialEval(op, Some(inst), inst.scope)
      }.toMap
      inst.invokeMethod(methodName, args)
    //partially eval inputs
    case TestGate(gateType, inputs) =>
      val evaluatedInputs = inputs.map(input => partialEval(input, instance, scope))
      if (evaluatedInputs.forall(_.isInstanceOf[FuzzyValue])) { //all inputs must be vals to compute gate result
        val inputValues = evaluatedInputs.map(_.asInstanceOf[FuzzyValue].value.asInstanceOf[Number].doubleValue())
        gateType.toUpperCase match {
          case "AND" =>
            FuzzyValue(inputValues.min)
          case "OR"  =>
            FuzzyValue(inputValues.max)
          case "NOT" =>
            if (inputValues.size != 1) throw new Exception("NOT gate expects exactly one input")
            FuzzyValue(1.0 - inputValues.head)
          case _ =>
            throw new Exception(s"Unknown gate type: $gateType")
        }
      } else {
        TestGate(gateType, evaluatedInputs.map(_.asInstanceOf[FuzzyOperation])) //return
      }
    //partially eval IfTrue condition
    case IfTrue(condition, thenBranch, elseBranch) =>
      val condValue = partialEval(condition, instance, scope)
      condValue match {
        case FuzzyValue(true)  =>
          partialEval(thenBranch, instance, scope)
        case FuzzyValue(false) =>
          partialEval(elseBranch, instance, scope)
        case _ => //both branches gets evaluated
          val partialThen = partialEval(thenBranch, instance, scope)
          val partialElse = partialEval(elseBranch, instance, scope)
          //partially evaluated IfTrue construct
          IfTrue(condValue.asInstanceOf[FuzzyOperation], partialThen.asInstanceOf[FuzzyOperation], partialElse.asInstanceOf[FuzzyOperation])
      }
    //partially eval comparision
    case GreaterEqual(left, right) =>
      val v1 = partialEval(left, instance, scope)
      val v2 = partialEval(right, instance, scope)
      (v1, v2) match {
        case (FuzzyValue(a: Number), FuzzyValue(b: Number)) =>  //if both are vals, computer comparision
          FuzzyValue(a.doubleValue() >= b.doubleValue())
        case _ => //partially evaluated >=
          GreaterEqual(v1.asInstanceOf[FuzzyOperation], v2.asInstanceOf[FuzzyOperation])
      }
    case _ => op //if no simplification left
  }
  //simplify add
  def simplifyAdd(v1: Any, v2: Any): Any = (v1, v2) match {
    case (FuzzyValue(a: Number), FuzzyValue(b: Number)) => //compute sum if both are numbers
      FuzzyValue(a.doubleValue() + b.doubleValue())
    case _ =>
      Add(toFuzzyOperation(v1), toFuzzyOperation(v2)) //add operation w/ partial
  }
  //simplify subtract
  def simplifySubtract(v1: Any, v2: Any): Any = (v1, v2) match {
    case (FuzzyValue(a: Number), FuzzyValue(b: Number)) => //computer diff if both are numbers
      FuzzyValue(a.doubleValue() - b.doubleValue())
    case _ =>
      Subtract(toFuzzyOperation(v1), toFuzzyOperation(v2)) //subtract operation w/ partial
  }
  //simplify multiply
  def simplifyMultiply(v1: Any, v2: Any): Any = (v1, v2) match {
    case (FuzzyValue(a: Number), FuzzyValue(b: Number)) => //computer product if both are numbers
      FuzzyValue(a.doubleValue() * b.doubleValue())
    case (FuzzyValue(a: Number), Multiply(FuzzyValue(b: Number), cOp)) => //combining constants
      Multiply(FuzzyValue(a.doubleValue() * b.doubleValue()), cOp)
    case (Multiply(FuzzyValue(a: Number), bOp), FuzzyValue(c: Number)) => //combining constants
      Multiply(FuzzyValue(a.doubleValue() * c.doubleValue()), bOp)
    case (FuzzyValue(a: Number), op: FuzzyOperation) => //multiply const w/ operation
      Multiply(FuzzyValue(a.doubleValue()), op)
    case (op: FuzzyOperation, FuzzyValue(a: Number)) => //multiply operation w/ const
      Multiply(FuzzyValue(a.doubleValue()), op)
    case _ => Multiply(toFuzzyOperation(v1), toFuzzyOperation(v2)) //return w/ partial eval
  }
  //simplify divide
  def simplifyDivide(v1: Any, v2: Any): Any = (v1, v2) match {
    case (FuzzyValue(a: Number), FuzzyValue(b: Number)) =>
      if (b.doubleValue() == 0) throw new Exception("Division by zero") //compute division if both are numbers
      else FuzzyValue(a.doubleValue() / b.doubleValue())
    case _ =>
      Divide(toFuzzyOperation(v1), toFuzzyOperation(v2)) //returning w/ partial
  }
  //helper function to convert values to FuzzyOperation
  def toFuzzyOperation(value: Any): FuzzyOperation = value match {
    case op: FuzzyOperation => op
    case v                  => FuzzyValue(v)
  }
}
