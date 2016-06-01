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

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = ???
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