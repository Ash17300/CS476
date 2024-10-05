import scala.collection.mutable

object FuzzyEval {
  //based on lecture format ish
  enum FuzzyOperation:
    case FuzzyValue(value: Double) //constant value between 0 & 1
    case FuzzyVariable(name: String)
    case FuzzyUnion(p1: FuzzyOperation, p2: FuzzyOperation) //fuzzy union operation
    case FuzzyIntersection(p1: FuzzyOperation, p2: FuzzyOperation) //fuzzy intersection operation
    case FuzzyComplement(p: FuzzyOperation) //fuzzy compliment operation
    case Assign(variable: FuzzyVariable, value: FuzzyValue) // fuzzy assignment operation


  //table for all the values and evaluates all the expressions
  def eval(exp: FuzzyOperation)(using env: mutable.Map[String, Double]): Double = exp match
    case FuzzyOperation.FuzzyValue(value) => value
    case FuzzyOperation.FuzzyVariable(name) => env.getOrElse(name, throw new Exception(s"Variable $name not found"))
    case FuzzyOperation.FuzzyUnion(p1, p2) => math.max(eval(p1), eval(p2))
    case FuzzyOperation.FuzzyIntersection(p1, p2) => math.min(eval(p1), eval(p2))
    case FuzzyOperation.FuzzyComplement(p) => 1.0 - eval(p)
    case FuzzyOperation.Assign(variable, value) =>
      env(variable.name) = eval(value) //stores the value to the variable
      eval(value)
}