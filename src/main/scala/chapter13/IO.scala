package chapter13

import chapter11.Monad

sealed trait IO[A] {
  self =>
  def run: A
  // def map[B](f: A => B): IO[B] = new IO[B] { def run = f(self.run) }
  //def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] { def run = f({self.run}).run }
  def map[B](f: A => B): IO[B] = new IO[B] { def run = f({
    println("M - Calling self.run")
    val a = self.run
    println("M - Result: " + a)
    a
  }) }
  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
    def run = {
      val result = f({
        println("FM - Calling self.run")
        val a = self.run
        println("FM - Result: " + a)
        a
      })
      println("FM - Calling result.run")
      val b = result.run
      println("FM - Result: " + b)
      b
    }
  }
}

object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
  def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
  def apply[A](a: => A): IO[A] = unit(a)
}

object IOFunctions {
  def ReadLine: IO[String] = IO { scala.io.StdIn.readLine }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }
}
