package chapter11

import chapter7.Par.Par
import chapter7._
import chapter8.Gen
import scala.language.higherKinds

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List(): List[A]))((ma, mla) => map2(ma, mla)((a, la) => a :: la))

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = sequence(la map f)

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = n match {
    case 0 => unit(List[A]())
    case n => map2(ma, replicateM(n - 1, ma))(_ :: _)
  }

  def product[A, B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](as: List[A])(f: A => M[Boolean]): M[List[A]] =
    as.foldRight(unit(List(): List[A]))((a, mla) => map2(f(a), mla)((p, la) => if (p) a :: la else la))

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = a => flatMap(f(a))(g)

  def flatMapViaCompose[A, B](ma: M[A])(f: A => M[B]): M[B] = compose(identity[M[A]], f)(ma)

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity[M[A]])

  def flatMapViaJoinAndMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

  def composeViaJoinAndMap[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = a => join(map(f(a))(g))

  /*def doWhile[A](a: M[A])(cond: A => M[Boolean]): M[Unit] = for {
    a1 <- a
    ok <- cond(a1)
    _ <- if (ok) doWhile(a)(cond) else unit(())
  } yield ()*/

  /*def forever[A,B](a: M[A]): M[B] = {
    lazy val t: M[B] = forever(a)
    flatMap(a)(_ => t)
  }*/

  def forever[A,B](a: M[A]): M[B] = {
    flatMap(a)(_ => forever(a))
  }

  def foldM[A,B](l: Stream[A])(z: B)(f: (B,A) => M[B]): M[B] =
    l match {
      case h #:: t => flatMap(f(z,h))(z2 => foldM(t)(z2)(f))
      case _ => unit(z)
    }

  def skip[A](a: M[A]) : M[Unit] = map(a)(_ => Unit)

  def foldM_[A,B](l: Stream[A])(z: B)(f: (B,A) => M[B]): M[Unit] =
    skip { foldM(l)(z)(f) }

  def foreachM[A](l: Stream[A])(f: A => M[Unit]): M[Unit] =
    foldM_(l)(())((u,a) => skip(f(a)))
}

object SampleMonads {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)

    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)

    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)

    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }

  /* val stateMonad = new Monad[State(A)] {
    def unit[A](a: => A): State[A, C] = ???

    def flatMap[A,B](ma: State[A, C])(f: A => State[B, D]): State[B, D] = ???
  }*/
}