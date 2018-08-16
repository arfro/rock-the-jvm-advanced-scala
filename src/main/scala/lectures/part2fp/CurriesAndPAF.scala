package lectures.part2fp

object CurriesAndPAF extends App {

  //curried functions
  val supperAdder: Int => Int => Int =
    x => y => x + y // curried function

  val add3 = supperAdder(3) // Int => Int = y => 3 + y
  println(add3(4))
  println (supperAdder(4)(3))

  def curriedAdder(x: Int)(y: Int): Int = x + y // curried method!

  val add4: Int => Int = curriedAdder(4) // this will not work without type annotation Int => Int
  // when you call a method (def) you need to supply all parameters.
  // to work around it we convert a method to a function of type Int => Int using val. We want to use function values
  // in HOF. We cannot use methods in HOF unless they're converted to val function.
  // Function != method is due to JVM limitations - method is only a class instance member
  // !! That convertion is called LIFTING or ETA expansion !!

}
