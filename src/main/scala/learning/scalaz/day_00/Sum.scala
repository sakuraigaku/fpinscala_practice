package learning.scalaz.day_00

import learning.scalaz.day_00.Sum._

object RunSum extends App {

  implicit val intMonoid = IntMonoid

  val result: Int = sum(List(1, 2, 3, 4))

  print(result)
}

object Sum {
  def sum[A: Monoid](xs: List[A]): A = {
    val m = implicitly[Monoid[A]]
    xs.foldLeft(m.mzero)(m.mapend)
  }

  trait Monoid[A] {
    def mapend(a1: A, a2: A): A

    def mzero: A
  }

  object IntMonoid extends Monoid[Int] {
    def mapend(a: Int, b: Int): Int = a + b

    def mzero: Int = 0
  }

  object StringMonoid extends Monoid[String] {
    def mapend(a: String, b: String): String = a + b

    def mzero: String = ""
  }

}

