package chapter10

import chapter3.{Branch, Leaf, Tree}

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa : F[A]): List[A] =
    // foldRight(fa)(List[A]())(_ :: _)
     foldMap(fa)(x => List(x))(SampleMonoids.listMonoid)
}

object SampleFoldables {

  def foldableList = new Foldable[List] {
    def foldRight[A,B](as: List[A])(z: B)(f: (A,B) => B): B = as.foldRight(z)(f)
    def foldLeft[A,B](as: List[A])(z: B)(f: (B,A) => B): B = as.foldLeft(z)(f)
    def foldMap[A,B](as: List[A])(f: A => B)(mb: Monoid[B]): B = Monoid.foldMap(as, mb)(f)
  }

  def foldableIndexedSeq = new Foldable[IndexedSeq] {
    def foldRight[A,B](as: IndexedSeq[A])(z: B)(f: (A,B) => B): B = as.foldRight(z)(f)
    def foldLeft[A,B](as: IndexedSeq[A])(z: B)(f: (B,A) => B): B = as.foldLeft(z)(f)
    def foldMap[A,B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = as.map(f).fold(mb.zero)(mb.op)
      //Monoid.foldMap(as.toList, mb)(f)
    // this should be imperformant since it expands to:
    //as.toList.map(f).fold(mb.zero)(mb.op)
  }

  def foldableStream = new Foldable[Stream] {
    def foldRight[A,B](as: Stream[A])(z: B)(f: (A,B) => B): B = as.foldRight(z)(f)
    def foldLeft[A,B](as: Stream[A])(z: B)(f: (B,A) => B): B = as.foldLeft(z)(f)
    def foldMap[A,B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B = as.map(f).fold(mb.zero)(mb.op)
    //Monoid.foldMap(as.toList, mb)(f)
  }

  def foldableOption = new Foldable[Option] {
    def foldRight[A,B](as: Option[A])(z: B)(f: (A,B) => B): B = as.foldRight(z)(f)
    def foldLeft[A,B](as: Option[A])(z: B)(f: (B,A) => B): B = as.foldLeft(z)(f)
    def foldMap[A,B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as.map(f).getOrElse(mb.zero)
  }

  def foldableTree = new Foldable[Tree] {
    def foldRight[A,B](as: Tree[A])(z: B)(f: (A,B) => B): B = as match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }

    def foldLeft[A,B](as: Tree[A])(z: B)(f: (B,A) => B): B = as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }

    def foldMap[A,B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case Leaf(a) => f(a)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }
  }
}
