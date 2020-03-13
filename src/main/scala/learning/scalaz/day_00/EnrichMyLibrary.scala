package learning.scalaz.day_00

import learning.scalaz.day_00.Sum.{IntMonoid, Monoid, StringMonoid}
import EnrichMyLibrary._

object RunEnrichMyLibrary extends App {

  implicit val intMonoid = IntMonoid
  implicit val stringMonoid = StringMonoid

  println(3 |+| 4)
  println("aaa" |+| "bbb")
}

object EnrichMyLibrary {

  trait MonoidOp[A] {
    val F: Monoid[A]
    val value: A

    def |+|(a2: A) = F.mapend(value, a2)
  }

  implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
    override val F: Monoid[A] = implicitly[Monoid[A]]
    override val value: A = a
  }

}