package chapter13

import scala.language.higherKinds
import scala.language.reflectiveCalls

import chapter7.Nonblocking.Par
import chapter11.Monad

sealed trait Free[F[_], A]{
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))
}

case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {
  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  def freeMonad[F[_]] = new Monad[({type f[a] = Free[F,a]})#f] {
    def unit[A](a: => A): Free[F, A] = Return(a)
    def flatMap[A, B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = FlatMap(ma, f)
  }

  @annotation.tailrec
  def runTrampoline[A](ffa: Free[Function0,A]): A = ffa match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(y, g) => runTrampoline(y flatMap (a => g(a).flatMap(f)))
    }
    case _ => ??? // dummy case to silence compiler false-positive warning
  }

  @annotation.tailrec
  def step[F[_],A](ffa: Free[F,A])(implicit F: Monad[F]): Free[F,A] = ffa match {
    case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a).flatMap(g)))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => ffa
  }

  def run[F[_],A](ffa: Free[F,A])(implicit F: Monad[F]): F[A] = step(ffa) match {
    case Return(a) => F.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f) => x match {
      case Suspend(r) => F.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }
}