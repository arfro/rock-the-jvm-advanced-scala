package lectures.part1

object AdvancedPatternMatching extends App {

  // to be able to use class (not a case class) in pattern match you need to define a companion object with
  // 'unapply' method
  class Person(val age: Int, val name: String)
  object Person{
    def unapply(person: Person): Option[(Int, String)] = Some((person.age, person.name))
    def unapply(age: Int): Option[String] = Some(
      if (age > 18) "adult"
      else "minor"
    )
  }

  val bob = new Person(23, "bob")
  val greeting = bob match {
    case Person(a, n) => s"Hi $n youre $a years old"
  }
  val legalStatus = bob.age match {
    case Person(status) => s"legal status is $status"
  }

  println(greeting)
  println(legalStatus)

  /*
  Exercise
  */
   object SingleDigit {
    def unapply(arg: Int): Boolean = arg < 10
  }
  object Even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }
  val n: Int = 44
  val mathProperty = n match {
    case SingleDigit() => "single"
    case Even() => "even"
    case _ => "none"
  }

  print(mathProperty)
}