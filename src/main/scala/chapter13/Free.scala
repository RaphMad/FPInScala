package chapter13

import scala.language.higherKinds
import scala.language.reflectiveCalls
import chapter7.Nonblocking.Par
import chapter11.Monad
import chapter13.Translate.~>

sealed trait Free[F[A], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))
}

case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[A], A](s: F[A]) extends Free[F, A]
case class FlatMap[F[A], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {
  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  def freeMonad[F[A]] = new Monad[({type f[a] = Free[F, a]})#f] {
    def unit[A](a: => A): Free[F, A] = Return(a)
    def flatMap[A, B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = FlatMap(ma, f)
  }

  @annotation.tailrec
  def runTrampoline[A](ffa: Free[Function0, A]): A = ffa match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(y, g) => runTrampoline(y flatMap (a => g(a).flatMap(f))) // isnt this case an endless recursion?
    }
    case _ => ??? // dummy case to silence compiler false-positive warning
  }

  @annotation.tailrec
  def step[F[_], A](ffa: Free[F, A]): Free[F, A] = ffa match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a).flatMap(g)))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => ffa
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit gMonad: Monad[G]): G[A] = step(free) match {
    case Return(a) => gMonad.unit(a)
    case Suspend(r) => t(r)
    case FlatMap(Suspend(r), f) => gMonad.flatMap(t(r))(a => runFree(f(a))(t))
    case _ => sys.error("Impossible; `step` eliminates these cases")
  }

  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A): Function0[A] =
      () => a
    def flatMap[A, B](a: Function0[A])(f: A => Function0[B]): Function0[B] =
      () => f(a())()
  }

  implicit val parMonad = new Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)
    def flatMap[A, B](a: Par[A])(f: A => Par[B]) = Par.fork {
      Par.flatMap(a)(f)
    }
  }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A = runFree[Console, Function0, A](a)(Translate.consoleToFunction0)
  def runConsolePar[A](a: Free[Console, A]): Par[A] = runFree[Console, Par, A](a)(Translate.consoleToPar)

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[A] = Free[G, A]

    val transformer = new (F ~> FreeG) {
      def apply[A](a: F[A]): Free[G, A] = Suspend {
        fg(a)
      }
    }
    runFree(f)(transformer)(freeMonad[G])
  }

  def runConsole[A](a: Free[Console, A]): A = runTrampoline {
    translate(a)(new (Console ~> Function0) {
      def apply[A](c: Console[A]) = c.toThunk
    })
  }
}
