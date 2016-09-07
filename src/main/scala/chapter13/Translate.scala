package chapter13

import chapter7.Nonblocking.Par

import scala.language.higherKinds

trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }

object Translate {
  type ~>[F[_], G[_]] = Translate[F, G]

  val consoleToFunction0 = new (Console ~> Function0) { def apply[A](a: Console[A]) = a.toThunk }
  val consoleToPar = new (Console ~> Par) { def apply[A](a: Console[A]) = a.toPar }
}
