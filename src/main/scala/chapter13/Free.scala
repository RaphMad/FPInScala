package chapter13

import chapter11.Monad

import scala.language.higherKinds
import chapter7.Nonblocking.Par

sealed trait Free[F[_], A]{
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))
}

case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object FreeTypes {
  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  def freeMonad[F[_]] = new Monad[({type f[a] = Free[F,a]})#f] {
    def unit[A](a: => A): Free[F, A] = Return(a)
    def flatMap[A, B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = FlatMap(ma, f)
  }
}