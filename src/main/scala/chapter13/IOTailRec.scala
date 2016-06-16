package chapter13

import chapter11.Monad

/**
  * Created by rmader on 16.06.2016.
  */
sealed trait IOTailRec[A] {
  def flatMap[B](f: A => IOTailRec[B]): IOTailRec[B] = FlatMap(this, f)
  def map[B](f: A => B): IOTailRec[B] = flatMap(f andThen (Return(_)))
}
case class Return[A](a: A) extends IOTailRec[A]
case class Suspend[A](resume: () => A) extends IOTailRec[A]
case class FlatMap[A,B](sub: IOTailRec[A], k: A => IOTailRec[B]) extends IOTailRec[B]

object IOTailRec extends Monad[IOTailRec] {
  def unit[A](a: => A): IOTailRec[A] = new IOTailRec[A] { def run = a }
  def flatMap[A, B](fa: IOTailRec[A])(f: A => IOTailRec[B]) = fa flatMap f
  def apply[A](a: => A): IOTailRec[A] = unit(a)
}

object IOTailRecFunctions {
  //def ReadLine: IO[String] = IO { scala.io.StdIn.readLine }
  def ReadLine: IOTailRec[String] = Suspend(scala.io.StdIn.readLine)
  def PrintLine(s: String): IOTailRec[Unit] = Suspend(() => Return(println(s)))

  /*@annotation.tailrec
  def run[A](io: IOTailRec[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => flatMap(g(a))(f)))
    }
  }*/
}

