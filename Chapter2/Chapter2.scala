/**
 * Created by azarnan1 on 2015-05-19.
 */
object Chapter2 {

  def abs(n: Int): Int =
    if (n<0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(x, abs(x))
  }

  //**************************************************
  def factorial(n: Int): Int ={
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n == 0)
        acc
      else
        go(n-1, n*acc)
    }

    go(n,1)
  }

  private def formatFact(x: Int) = {
    val msg = "The abs of %d is %d"
    msg.format(x, factorial(x))
  }

  //**************************************************
  //#2.1
  private def formatMath(o: String, x: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(o, x, f(x))
  }

  private def formatFib(x: Int) = {
    val msg = "The Fibonacci at %d is %d"
    msg.format(x, fib(x))
  }

  private def fib(n:Int):Int ={
    //this version is not tail recursive
    //@annotation.tailrec
    def go(n:Int): Int = {
      if (n==0)
        0
      else if (n==1)
        1
      else
        go(n-1) + go(n-2)
    }
    go(n)
  }

  def fib2(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }

  //#2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean= {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n>=as.length-1)
        true
      else if (!ordered(as(n),as(n+1)))
        false
      else loop(n+1)

    loop(0)
  }

  //#2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  //#2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
  //**************************************************
  def main(args: Array[String]): Unit = {
    println(formatAbs(-90))
    println(formatFact(4))
    println(formatFib(1))

    println(formatMath("my fib", 6, fib))
    println(formatMath("my fib2", 6, fib2))

    println(
      isSorted(Array(1,2,-3), (x:Int, y:Int) => x<y)
    )

  }
}

