package learning.scalaz.day_00

import FoldLeft._
import learning.scalaz.day_00.FoldLeft.FoldLeft.FoldLeftList
import learning.scalaz.day_00.Sum.{IntMonoid, Monoid, StringMonoid}

object RunFoldLeft extends App {

  implicit val intMonoid = IntMonoid
  implicit val stringMonoid = StringMonoid
  val multiMonoid: Monoid[Int] = new Monoid[Int] {
    override def mapend(a1: Int, a2: Int): Int = a1 * a2

    override def mzero: Int = 1
  }

  //  val result = sum(List(1, 2, 3, 4))
  val result = sum(List("a", "b", "c"))

  print(result)
}


object FoldLeft {

  trait FoldLeft[F[_]] {
    def foldLeft[A, B](xs: F[A], b: B, f: (B, A) => B): B
  }

  object FoldLeft {

    implicit val FoldLeftList: FoldLeft[List] = new FoldLeft[List] {
      override def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B) = xs.foldLeft(b)(f)
    }

  }

  def sum[M[_] : FoldLeft, A: Monoid](xs: M[A]): A = {
    val m = implicitly[Monoid[A]]
    val fl = implicitly[FoldLeft[M]]
    fl.foldLeft(xs, m.mzero, m.mapend)
  }

}
