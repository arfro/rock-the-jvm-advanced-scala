package exercises

trait MySet[A] extends (A => Boolean) {
  // contains
  // + ret new MySet
  // ++ concatenate another set (union)
  // map flatMap and filter foreach filter
  def apply(elem: A): Boolean = contains(elem)
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(elems: MySet[A]): MySet[A]
  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(pred: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  def remove(elem: A): MySet[A]
  def intersect(anotherSet: MySet[A]): MySet[A]
  def diff(anotherSet: MySet[A]): MySet[A]

}

class EmptySet[A] extends MySet[A] {
  override def apply(elem: A): Boolean = false

  override def remove(elem: A): MySet[A] = this
  override def intersect(anotherSet: MySet[A]): MySet[A] = this
  override def diff(anotherSet: MySet[A]): MySet[A] = this

  override def contains(elem: A): Boolean = false
  override def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)
  override def ++(elems: MySet[A]): MySet[A] = elems

  override def map[B](f: A => B): MySet[B] = new EmptySet[B]
  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  override def filter(pred: A => Boolean): MySet[A] = this
  override def foreach(f: A => Unit): Unit = ()
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A]{
  override def contains(elem: A): Boolean =
    elem == head || tail.contains(elem)
  override def +(elem: A): MySet[A] =
    if(this.contains(elem)) this
    else new NonEmptySet[A](elem, this)

  override def ++(elems: MySet[A]): MySet[A] =
    // [1 2 3] ++ [4 5]
    tail ++ elems + head


  override def map[B](f: A => B): MySet[B] =
    (tail map f) + f(head)
  override def flatMap[B](f: A => MySet[B]): MySet[B] =
    (tail flatMap f) ++ f(head)
  override def filter(pred: A => Boolean): MySet[A] = {
    val filteredTail = tail filter pred
    if(pred(head)) filteredTail + head
    else filteredTail
  }
  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def remove(elem: A): MySet[A] = this.filter(thisEl => thisEl != elem)
  override def intersect(anotherSet: MySet[A]): MySet[A] = this.flatMap(elemA => anotherSet.filter(elemB => elemA == elemB))
  override def diff(anotherSet: MySet[A]): MySet[A] = this.flatMap(elemA => anotherSet.filter(elemB => elemA != elemB))
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    def buildSet(valseq: Seq[A], acc: MySet[A]): MySet[A] = {
      if(valseq.isEmpty) acc
      else buildSet(valseq.tail, acc + valseq.head)
    }
    buildSet(values.toSeq, new EmptySet[A])
  }
}

object Play extends App {
  val s = MySet(1, 2, 3, 4)
  val s1 = MySet(3,4,5,6)
  s foreach(println)
  s + 5 ++ MySet(-1, -2) foreach(println)
  s + 5 + 5 ++ MySet(-1, -2) foreach(println)
  s map (_*10) foreach(println)
  s flatMap (x => MySet(x, x * 100)) foreach(println)
  s filter (_%2 == 0) foreach(println)
  print("-------\n")
  s.remove(4) foreach(println)
  s.intersect(s1) foreach(println)
  s.diff(s1) foreach(println)
}