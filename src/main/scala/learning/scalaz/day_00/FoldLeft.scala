package learning.scalaz.day_00

import FoldLeft._
import learning.scalaz.day_00.Sum.{IntMonoid, Monoid, StringMonoid}

object RunFoldLeft extends App {

  implicit val intMonoid = IntMonoid
  implicit val stringMonoid = StringMonoid

  val result = sum(List("a", "b", "c"))

  print(result)
}

object FoldLeft {

  object FoldLeftList {

    def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B) = xs.foldLeft(b)(f)

  }

  def sum[A: Monoid](xs: List[A]): A = {
    val m = implicitly[Monoid[A]]
    FoldLeftList.foldLeft(xs, m.mzero, m.mapend)
  }

}
