package chapter13.IOTailRec

import chapter11.Monad

/**
  * Created by rmader on 16.06.2016.
  */
sealed trait IO[A] {
  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
}
case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]

object IOTailRec extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
  def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
  def apply[A](a: => A): IO[A] = unit(a)
}

object IOTailRecFunctions {
  //def ReadLine: IO[String] = IO { scala.io.StdIn.readLine }
  def ReadLine: IO[String] = Suspend(scala.io.StdIn.readLine)
  def PrintLine(s: String): IO[Unit] = Suspend(() => Return(println(s)))

  @annotation.tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a).flatMap(f)))
    }
    case _ => ??? // dummy case to silence compiler false-positive warning
  }
}

