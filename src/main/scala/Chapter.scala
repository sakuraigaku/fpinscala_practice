object Run extends App {
//  println(Chapter2.fib(4))
  println(Chapter2.isSorted(Seq(4, 2, 3).toArray, testIsSorted))

  def testIsSorted(x: Int, y: Int): Boolean = x < y
}

object Chapter2 {
  def fib(n: Int): Int = {
    def go(n: Int, pre: Int, cur: Int): Int = if (n <= 0) pre else go(n - 1, cur, pre + cur)

    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(x: Int): Boolean = if (x == as.length - 1) true else if (ordered(as(x), as(x + 1))) go(x + 1) else false

    go(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def conpose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}

object Chaptar3 {

  sealed trait List[+A]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  case object Nil extends List[Nothing]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

}
