package chapter6

import scala.annotation.tailrec

trait RNG {
   def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

   def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
   }
}

object RNG {
   type Rand[+A] = RNG => (A, RNG)

   def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (num, newState) = rng.nextInt
      if (num < 0) (-(num + 1), newState)
      else (num, newState)
   }

   def between(rng: RNG)(start: Int, end: Int): Rand[Int] = {
      val (num, newState) = rng.nextInt

      if (num < start || num >= end) between(rng)(start, end)
      else rng => (num, newState)
   }

   def nextDouble(rng: RNG): (Double, RNG) = {
      val (num, newState) = rng.nonNegativeInt
      (num / (Int.MaxValue.toDouble + 1), newState)
   }

   def intDouble1(rng: RNG): ((Int, Double), RNG) = {
      val (int, newState) = rng.nextInt
      val (dou, newState2) = nextDouble(newState)
      ((int, dou), newState2)
   }

   def intDouble2(rng: RNG): ((Int, Double), RNG) = {
      rng.nextInt match {
         case (int, s2) =>
            val (dou, s3) = nextDouble(s2)
            ((int, dou), s3)
      }
   }

   def doubleIntBp(rng: RNG): ((Double, Int), RNG) = {
      val ((int, dou), s2) = intDouble1(rng)
      ((dou, int), s2)
   }

   def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, r1) = nextDouble(rng)
      val (d2, r2) = nextDouble(r1)
      val (d3, r3) = nextDouble(r2)
      ((d1, d2, d3), r3)
   }

   def intsRecurs(count: Int)(rng: RNG): (List[Int], RNG) = {
      @tailrec
      def goInts(collector: List[Int], current: RNG, countLeft: Int): (List[Int], RNG) = {
         if (countLeft > 0) {
            val (newInt, newRNG) = current.nextInt
            goInts(newInt :: collector, newRNG, countLeft - 1)
         }
         else (collector, current)
      }

      goInts(List(), rng, count)
   }

   // TODO
   def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      List.tabulate(count)(c => {
         val (newInt, newRNG) = rng.nextInt
         ???
      })
      ???
   }

   val int: Rand[Int] = _.nextInt
   val double: Rand[Double] = rng => nextDouble(rng)

   def boolean(rng: RNG): (Boolean, RNG) =
      rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

   def unit[A](a: A): Rand[A] =
      rng => (a, rng)

   def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
         val (a, rng2) = s(rng)
         (f(a), rng2)
      }

   def nonNegativeEven: Rand[Int] =
      map(nonNegativeInt)(i => i - i % 2)

   def doubleViaMap(rng: RNG): Rand[Double] =
      map(int)(i => i / (Int.MaxValue.toDouble + 1))

   val _double: Rand[Double] =
      map(nonNegativeInt)(nonNegativeIntToDouble)

   def sequence1[A](fs: List[Rand[A]]): Rand[List[A]] = {

      def littleHelper(acc: (List[A], RNG), rc: Rand[A]): (List[A], RNG) = {
         val (ls, rng1) = acc
         val (v, rngNew) = rc(rng1)
         (v :: ls, rngNew)
      }

      rng => fs.foldLeft((List[A](), rng))(littleHelper)

   }

   def nonNegativeLessThan(n: Int): Rand[Int] =
      flatMap(nonNegativeInt)(i => {
         val mod = i % n
         if (i + (n - 1) - mod >= 0) unit(mod)
         else nonNegativeLessThan(n)
      })

   def nonNegativeIntToDouble(i: Int): Double =
      i / (Int.MaxValue.toDouble + 1)

   def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rnd => {
         val (a, rnd2) = ra(rnd)
         val (b, rnd3) = rb(rnd2)
         (f(a, b), rnd3)
      }

   /*
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    map(ra)(a => map(rb)(b => f(a, b)))
    */

   def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
      map2(ra, rb)((_, _))

   def intDouble2: Rand[(Int, Double)] =
      both(int, double)

   def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      ???

   def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rnd => {
         val (a, rnd2) = f(rnd)
         g(a)(rnd2)
      }

   case class State[S, +A](run: S => (A, S)) {

      def map[B](f: A => B): State[S, B] = State(s => {

         val (a, newState) = run(s)
         (f(a), newState)
      })

      def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {

         val (a, newState) = run(s)
         val (b, newerState) = rb.run(newState)

         (f(a, b), newerState)
      })

      def flatMap[B](g: A => State[S, B]): State[S, B] = State(s => {

         val (result, newState) = run(s)
         g(result).run(newState)

      })
   }

   object State {

      def unit[S, A](a: A): State[S, A] = State(s => (a, s))

      def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State(s => {
         fs match {
            case (h :: t) => {
               val (result, newState) = h.run(s)
               val (tail, lastState) = sequence(t).run(newState)

               (result :: tail, lastState)
            }
            case Nil => (Nil, s)
         }
      })

      def modify[S](f: S => S): State[S, Unit] = for {
         s <- get
         _ <- set(f(s))
      } yield ()

      def get[S]: State[S, S] = State(s => (s, s))

      def set[S](s: S): State[S, Unit] = State(_ => ((), s))

   }

   sealed trait Input
   case object Coin extends Input
   case object Turn extends Input

   case class Machine(locked: Boolean, candies: Int, coins: Int) {

   }

   object Machine {

      def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
         _ <- State.sequence(inputs map (State.modify[Machine] _ compose Machine.update))
         s <- State.get
      } yield (s.coins, s.candies)

      def update = (i: Input) => (s: Machine) =>
         (i, s) match {
            case (_, Machine(_, 0, _))        => s
            case (Coin, Machine(false, _, _)) => s
            case (Turn, Machine(true, _, _))  => s
            case (Coin, Machine(true, candy, coin)) =>
               Machine(false, candy, coin + 1)
            case (Turn, Machine(false, candy, coin)) =>
               Machine(true, candy - 1, coin)
         }

      def simulateMachine(input: Input, m: Machine): Machine = input match {

         case Coin => m match {
            // rule 1
            case Machine(true, candies, coins) if coins > 0 => Machine(false, coins + 1, candies)

            // rule 3b
            case Machine(true, candies, coins)              => Machine(true, coins, candies)

            // rule 4a
            case Machine(lockState, 0, coins)               => Machine(lockState, coins, 0)
         }

         case Turn => m match {
            // rule 2
            case Machine(false, coins, candies) => Machine(true, coins, candies - 1)

            // rule 3a
            case Machine(true, candies, coins)  => Machine(true, coins, candies)

            // rule 4a
            // case Machine(lockState, 0, coins)   => Machine(lockState, coins, 0)
         }
      }
   }
}
