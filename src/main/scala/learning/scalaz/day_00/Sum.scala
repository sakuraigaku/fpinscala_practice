package learning.scalaz.day_00

import learning.scalaz.day_00.Sum._

object RunSum extends App {

  implicit val intMonoid = IntMonoid

  val multiMonoid: Monoid[Int] = new Monoid[Int] {
    override def mapend(a1: Int, a2: Int): Int = a1 * a2

    override def mzero: Int = 1
  }

  val result: Int = sum(List(1, 2, 3, 4))(multiMonoid)

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

