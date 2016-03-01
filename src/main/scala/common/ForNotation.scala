package common

import scala.Option._
import scala._

object ForNotation {

   def translationToFor = {

      // m1-m3 can be any type that implements flatMap() and map() (= a monad)
      val m1 = List(1, 2, 3)
      val m2 = List(4, 5, 6)
      val m3 = List(7, 8, 9)

      m1.flatMap(x => m2.flatMap(y => m3.map(z => (x, y, z))))

      // other formatting
      m1.flatMap(x =>
         m2.flatMap(y =>
            m3.map(z =>
               (x, y, z))))

      // pure 'mechanic' translation to for (note that 'm1' and 'x' switched places, but the arrow direction also switched)
      for {
         x <- m1
         y <- m2
         z <- m3
      } yield (x, y, z)
   }

   // what a for block actually does depends on the flatmap / map implementation of the monad:
   def implementations = {

      // using built-in functions is cheating, but we defined those from scratch in the exercises
      def listMap[A, B](list: List[A])(f: A => B): List[B] = list map f
      def listFlatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list flatMap f

      def optionMap[A, B](option: Option[A])(f: A => B): Option[B] = option match {

         case Some(a) => Some(f(a))
         case None    => None
      }

      def optionFlatMap[A, B](option: Option[A])(f: A => Option[B]): Option[B] = option match {

         case Some(a) => f(a)
         case None    => None
      }

      type State[A, S] = S => (A, S)

      def stateMap[A, B, S](statement: State[A, S])(f: A => B): State[B, S] = s => {

         val (a, s1) = statement(s)
         (f(a), s1)
      }

      def stateFlatMap[A, B, S](statement: State[A, S])(f: A => State[B, S]): State[B, S] = s => {

         val (a, s1) = statement(s)
         f(a)(s1)
      }
   }
}
