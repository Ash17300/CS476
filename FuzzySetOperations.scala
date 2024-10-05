object FuzzySetOperations {
  //max of 2 values - union
  def fuzzyUnion(setA: Map[String, Double], setB: Map[String, Double]): Map[String, Double] = {
    val result = for {
      key <- setA.keys ++ setB.keys } //loop for both sets
    yield key -> math.max(setA.getOrElse(key, 0.0), setB.getOrElse(key, 0.0)) //max value gets chosen from that
    result.toMap
  }
  //min of 2 values - intersection
  def fuzzyIntersection(setA: Map[String, Double], setB: Map[String, Double]): Map[String, Double] = {
    val result = for {
      key <- setA.keys ++ setB.keys }
    yield key -> math.min(setA.getOrElse(key, 0.0), setB.getOrElse(key, 0.0)) //min value gets chosen for each
    result.toMap
  }
  //compliment of 2 values
  def fuzzyComplement(setA: Map[String, Double]): Map[String, Double] = {
    val result = for {
      (key, value) <- setA }
    yield key -> (1.0 - value) //subtract 1 from each for opposite element
    result.toMap
  }
  //adding 2 sets
  def fuzzyAddition(setA: Map[String, Double], setB: Map[String, Double]): Map[String, Double] = {
    val result = for {
      key <- setA.keys ++ setB.keys }
    yield key -> math.min(1.0, setA.getOrElse(key, 0.0) + setB.getOrElse(key, 0.0)) // Add values, cap at 1
    result.toMap
  }
  //multiply 2 sets
  def fuzzyMultiplication(setA: Map[String, Double], setB: Map[String, Double]): Map[String, Double] = {
    val result = for {
      key <- setA.keys ++ setB.keys }
    yield key -> (setA.getOrElse(key, 0.0) * setB.getOrElse(key, 0.0)) // Multiply values
    result.toMap
  }
}
