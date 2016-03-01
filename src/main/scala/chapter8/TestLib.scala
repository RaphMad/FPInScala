/*package chapter8

import chapter5._
import chapter6._
import chapter8.Prop.{ TestCases, FailedCase, SuccessCount }

case class Gen[A](sample: State[RNG, A]) {

   def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample flatMap (f(_).sample))

   def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap (Gen.listOfN(_, this))

   def flatMap2[B](f: A => Gen[B]): Gen[B] = {

      val statement = sample.flatMap(a => f(a).sample)
      Gen(statement)
   }

   def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[A](forSize: Int => Gen[A]) {

   def flatMap[B](f: A => SGen[B]): SGen[B] = SGen[B](n => forSize(n) flatMap (a => f(a).forSize(n)))
}

object Gen {

   def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g listOfN unit(n))

   def choose(start: Int, stopExclusive: Int): Gen[Int] = {

      val param: State[RNG, Int] = State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))
      Gen(param)
   }

   def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

   def boolean: Gen[Boolean] = Gen(State(RNG.int).map(_ >= 0))

   def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {

      val listOfRands = List.fill(n)(g.sample)
      val randList = State.sequence(listOfRands)
      Gen(randList)
   }

   def double: Gen[Double] = Gen(State(RNG.double).map(d => d.abs / Double.MaxValue))

   def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean flatMap (if (_) g1 else g2)

   def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = double flatMap (d => if (d > g1._2) g1._1 else g2._1)
}

trait Prop {

   sealed trait Result {
      def isFalsified: Boolean
   }
   case object Passed extends Result {
      def isFalsified = false
   }
   case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
      def isFalsified = true
   }

   type MaxSize = Int
   case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
      def &&(p: Prop): Prop = Prop { (m, n, rng) =>
         run(m, n, rng) match {
            case Passed => p.run(m, n, rng)
            case failed => failed
         }
      }

      def ||(p: Prop): Prop = Prop { (m, n, rng) =>
         run(m, n, rng) match {
            case Passed => Passed
            case _      => p.run(m, n, rng)
         }
      }
   }

   def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

   def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
      (_, n, rng) =>
         randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
            case (a, i) => try {
               if (f(a)) Passed else Falsified(a.toString, i)
            }
            catch { case e: Exception => Falsified(buildMsg(a, e), i) }
         }.find(_.isFalsified).getOrElse(Passed)
   }

   def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

   def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
         s"generated an exception: ${e.getMessage}\n" +
         s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

   def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize(100))(f)

   def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (max, n, rng) =>

         val casesPerSize = (n + (max - 1)) / max

         val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))

         val prop: Prop = props.map(p => Prop { (max, _, rng) =>
            p.run(max, casesPerSize, rng)
         }).toList.reduce(_ && _)

         prop.run(max, n, rng)
   }
}

object Prop {

   type TestCases = Int
   type Result = Option[(FailedCase, SuccessCount)]
   type FailedCase = String
   type SuccessCount = Int
}*/
