package lectures.part2fp

import scala.util.Try

object PartialFunctions extends App {

  val aFunc = (x: Int) => x + 1 // Function1[Int, Int] OR Int => Int, this is defined on a whole Int domain, any Int can be passed to this function

  // sometimes we only want some values from the domain, say 1, 2, 5.. Not a great solution...
  val aFussyFunc = (x: Int) =>
    if (x == 1) 1
    else if (x == 2) 2
    else if (x == 3) 3
    else -1

  // but maybe a pattern match? If 7, will throw match exception
  val nicerFussyFunc = (x: Int ) => // total function
    x match {
      case 1 => 1
      case 2 => 2
      case 3 => 3
    }
  // {1, 2, 5} => Int - this is a partial function, it only applied to a subset of a given domain, here: 1, 2, 5 for all Ints


  val partialFun: PartialFunction[Int, Int] = {
    case 1 => 1
    case 2 => 2
    case 3 => 3
  } // partial function value, equivalent to above notation, they will act as proper functions..
  // so
  println(partialFun(3)) // will give 3
  //println(partialFun(22)) // crashes with match exception. They base on a pattern match.

  // partial functions utilities
  println(partialFun.isDefinedAt(55)) // will it work with 55? False
  println(partialFun.isDefinedAt(1)) // will it work with 1? True

  // we can lift it to a total function

  val lifted = partialFun.lift // will turn it to a total function of type Int => Option[Int]
  println(lifted(33)) // None
  println(lifted(1)) // Some(1)

  // add more cases to a partial function? here:
  val partialfunChain = partialFun.orElse[Int, Int] {
    case 55 => 55
  }
  print(partialfunChain(55)) // Some(55)
  print(partialfunChain(1)) // Some(1)

  // partial functions extend normal functions
  val totalFun: Int => Int = {
    case 1 => 1
  }

  // HOF accept partial functions as well
  val aMappedList = List(1,2,3).map{
    case 1 => 42
    case 2 => 66
    case 3 => 52
    // if case 3 was case 5 it would crash. 5 isn't
  }

  print(aMappedList) // 42, 66, 53

  // partial functions can only have one parameter type.

  // exercises
  // 1. construct a pf instance - instantiate partial function trait on spot (anon class)
  val ex1 = new PartialFunction[String, String] {
    override def apply(v1: String): String = v1 match {
      case "hi" => "hi how are you"
    }
    override def isDefinedAt(x: String): Boolean = Try(apply(x)).isSuccess
  }
  print(ex1.isDefinedAt("sdsad")) // false

  // 2. implement a small dumb chatbox (as a partial function)
  scala.io.Source.stdin.getLines().foreach(input => print(ex1(input)))
  //OR
  scala.io.Source.stdin.getLines().map(ex1).foreach(println)

}
