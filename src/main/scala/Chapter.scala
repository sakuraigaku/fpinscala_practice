object Run extends App {
  println(Chapter2.fib(4))
}

object Chapter2 {
  def fib(n: Int): Int = {
    def go(n: Int, pre: Int, cur: Int): Int = if (n <= 0) pre else go(n - 1, cur, pre + cur)
    go(n, 0, 1)
  }
}
