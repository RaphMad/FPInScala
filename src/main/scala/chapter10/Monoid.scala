package chapter10

import chapter10.SampleMonoids._
import chapter7.Nonblocking._
import chapter7.Nonblocking.Par.toParOps

trait Monoid[A] {
  self =>
  def op(a1: A, a2: A): A
  val zero: A

  // the dual Monoid has the arguments of the operation switched
  lazy val dual: Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = self.op(y, x)
    val zero = self.zero
  }
}

object Monoid {
  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as map f, m)

  // map to list of functions 'B => B', then combine then into a single function, then feed it the initial value
  // [f1, f2, f3] becomes f1(f2(f3()))
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    // type: B => B
    val monoid = SampleMonoids.endoMonoid[B]

    val func = foldMap(as, monoid)(a => f.curried(a))
    func(z)
  }

  // map to list of functions 'B => B', then combine then into a single function, then feed it the initial value
  // [f1, f2, f3] becomes f1(f2(f3()))
  def foldRight2[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(a => f.curried(a))(z)

  // flip arguments and use the dual endoMonoid: [f1, f2, f3] becomes f3(f2(f1()))
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B].dual)(a => (b => f(b, a)))(z)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.length match {
    case 0 => m.zero
    case 1 => f(v(0))
    case length => v.splitAt(length / 2) match {
      case (l, r) => m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]) = Par.fork(a1.map2(a2)(m.op))
    val zero = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    foldMapV(v, par(m))(Par.asyncF(f))
  }

  def parFoldMap2[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {

    val parMonoid = par(m)

    def go(v: IndexedSeq[A]): Par[B] = {
      v.length match {
        case 0 => parMonoid.zero
        case 1 => Par.asyncF(f)(v(0))
        case length => v.splitAt(length / 2) match {
          case (l, r) => parMonoid.op(go(l), go(r))
        }
      }
    }

    go(v)
  }

  def parFoldMap6[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(Par.asyncF(f))

  // we perform the mapping and the reducing both in parallel
  def parFoldMap4[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =

    Par.parMap(v)(f).flatMap { bs =>

      // to check: is this really parallel?
      // it seems like mapping is done in parallel,
      // but as soon as all mapping is complete reduce is done serially
      foldMapV(bs, par(m))(b => Par.unit(b))
    }

  def parFoldMap3[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {

    val parMonoid = par(m)

    def go(v: IndexedSeq[A]): Par[B] = {
      v.length match {
        case 0 => parMonoid.zero
        case 1 => Par.unit(f(v(0)))
        case length => v.splitAt(length / 2) match {
          case (l, r) => parMonoid.op(go(l), go(r))
        }
      }
    }

    go(v)
  }

  def isOrdered(v: IndexedSeq[Int]): Boolean = ???

  // This implementation detects only ascending order,
  // but you can write a monoid that detects both ascending and descending
  // order if you like.
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    // The empty sequence is ordered, and each element by itself is ordered.
    foldMapV(ints, exampleOrderMonoid)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }

  def wordCount(s: String): Int = {

    val wordSeparator = Part("", 0, "")
    val result = foldMapV(s, wcMonoid)(c => if (c.isWhitespace) wordSeparator else Stub(c.toString))

    wcMonoid.op(wordSeparator, wcMonoid.op(result, wordSeparator)) match {
        case Part(_, n, _) => n

        // can never happen! (or can it?)
        case _             => ???
      }
  }

  def orderedSimple(ints: IndexedSeq[Int]): Boolean = {
    // The empty sequence is ordered, and each element by itself is ordered.
    foldMapV(ints, simpleOrderMonoid)(i => Some((i, true))).forall(_._2)
  }

  def functionMonoid[A,B](mb: Monoid[B]): Monoid[A => B] = new Monoid[A => B]{
    def op(f1: A => B, f2: A => B) = (a: A) => mb.op(f1(a), f2(a))
    val zero = (a: A) => mb.zero
  }

  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(l: (A, B), r: (A, B)) = (ma.op(l._1, r._1), mb.op(l._2, r._2))
    val zero = (ma.zero, mb.zero)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      val zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]) = (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
      }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val monoid: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
    foldMapV(as, monoid)(a => Map(a -> 1))
  }

  def bag2[A](as: IndexedSeq[A]): Map[A, Int] = {
    val monoid: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
    foldMapV(as, monoid)(a => Map(a -> 1))
  }
}

object SampleMonoids {

  val simpleOrderMonoid = new Monoid[Option[(Int, Boolean)]] {

    def op(o1: Option[(Int, Boolean)], o2: Option[(Int, Boolean)]) =
      (o1, o2) match {
        case (Some((x, p)), Some((y, q))) => Some((y, p && q && x <= y))
        case (x, None) => x
        case (None, x) => x
      }

    val zero = None
  }

  // Our monoid tracks the minimum and maximum element seen so far
  // as well as whether the elements are so far ordered.
  val exampleOrderMonoid = new Monoid[Option[(Int, Int, Boolean)]] {

    def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
      (o1, o2) match {
        // The ranges should not overlap if the sequence is ordered.
        case (Some((x1, y1, p)), Some((x2, y2, q))) =>
          Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
        case (x, None) => x
        case (None, x) => x
      }

    val zero = None
  }

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    val zero = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1 compose a2
    val zero = identity[A] _
  }

  def sortMonoid = new Monoid[(List[Int], Boolean)] {
    def op(a1: (List[Int], Boolean), a2: (List[Int], Boolean)) = ???
    val zero = (Nil, true)
  }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(f1: (A => B), f2: (A => B)) = a => B.op(f1(a), f2(a))
    val zero = (_:A) => B.zero
  }

  sealed trait WC

  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def countWords(s: String): Integer = s.split(" ").length

  /*val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC) = (a1, a2) match {

      case (Stub(" "), Stub(" ")) => Stub(" ")
      case (Stub(l), Stub(" ")) => Part(l, 0, "")
      case (Stub(" "), Stub(r)) => Part("", 0, r)
      case (Stub(l), Stub(r)) => Stub(l + r)

      case (Stub(" "), Part(lStub, number, rStub)) => Part("", number + (if (lStub.isEmpty) 0 else 1), rStub)
      case (Stub(l), Part(lStub, number, rStub)) => Part(l + lStub, number, rStub)

      case (Part(lStub, number, rStub), Stub(" ")) => Part(lStub, number + (if (rStub.isEmpty) 0 else 1), "")
      case (Part(lStub, number, rStub), Stub(r)) => Part(lStub, number, rStub + r)

      case (Part(lStub1, number1, rStub1), Part(lStub2, number2, rStub2)) =>
        Part(lStub1, number1 + number2 + (if ((rStub1 + lStub2).isEmpty) 0 else 1), rStub2)
    }

    val zero = Stub("")
  }*/

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC) = (a1, a2) match {

      case (Stub(l), Stub(r)) => Stub(l + r)

      case (Stub(l), Part(lStub, number, rStub)) => Part(l + lStub, number, rStub)
      case (Part(lStub, number, rStub), Stub(r)) => Part(lStub, number, rStub + r)

      case (Part(lStub1, number1, rStub1), Part(lStub2, number2, rStub2)) =>
        Part(lStub1, number1 + number2 + (if ((rStub1 + lStub2).isEmpty) 0 else 1), rStub2)
    }

    val zero = Stub("")
  }
}
