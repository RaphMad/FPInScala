package chapter7

import java.util.concurrent._
import scala.Option._

package object par {

   val logger = (message: String) => println("log @" + new java.text.SimpleDateFormat("ss.SSS").format(new java.util.Date()) + ": " + message)
   // val logger = (message: String) => Unit

   type Par[A] = ExecutorService => Future[A]

   def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

   def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

   def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

   private case class UnitFuture[A](get: A) extends Future[A] {

      // constructor parameter 'get' implicitly creates a field called 'get'
      // it seems that this field is used for the implementation of 'Future.get()' (without parameters)

      def get(timeout: Long, units: TimeUnit) = get

      def isDone = true
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
   }

   def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =

      (es: ExecutorService) => {

         val af = a(es)
         val bf = b(es)

         UnitFuture(f(af.get, bf.get))
      }

   def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

   def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = {
      val result = map(pa)(f)
      join(result)
   }

   def join[A](pa: Par[Par[A]]): Par[A] = es => {
      val result = pa(es).get
      result(es)
   }

   def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] = flatMap(ppa)(pa => pa)

   // runs a, then b!
   def map2Serial[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = flatMap(pa)(a => flatMap(pb)(b => unit(f(a, b))))
   def map2Serial2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = flatMap(pa)(a => map(pb)(b => f(a, b)))

   def fork[A](a: => Par[A]): Par[A] = es => {

      val parent = Thread.currentThread().getName()

      es.submit(new Callable[A] {
         def call = {

            val child = Thread.currentThread().getName
            logger(s"spawned '$child' from '$parent', start calculation")

            val result = a(es).get
            logger(s"finished '$child' with result '$result'")

            result
         }
      })
   }

   def delay[A](fa: => Par[A]): Par[A] =
      es => fa(es)

   def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

   def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight[Par[List[A]]](unit(List()))((x, acc) => map2(x, acc)(_ :: _))

   def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
   }

   def parFilterNaive[A](as: List[A])(f: A => Boolean): Par[List[A]] = asyncF(as.filter)(f)

   def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {

      val listOfParOfOptions = as.map(asyncF(a => if (f(a)) Some(a) else None))
      val parOfListOfOptions = sequence(listOfParOfOptions)

      // map() is Par.map()
      // flatten() transforms lists of lists into single lists by concat-ing them
      // it also works for a lists of Options since Options can behave as empty or 1-element lists via its GenTraversable implementation
      map(parOfListOfOptions)(xs => xs.flatten)

      // alternative implementations:
      // map(parOfListOfOptions)(xs => xs.flatMap(x => x))
      // map(parOfListOfOptions)(xs => xs.filter(x => x.isDefined).map(x => x.get))
   }

   def sum(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.length <= 1)
         unit(ints.headOption getOrElse 0)
      else {
         val (l, r) = ints.splitAt(ints.length / 2)
         map2(fork(sum(l)), fork(sum(r)))(_ + _)
      }

   def fold[A](seq: IndexedSeq[A])(f: (A, A) => A, default: A)(forkUntil: Integer): Par[A] =
      if (seq.length <= 1)
         unit(seq.headOption getOrElse default)
      else {
         val (l, r) = seq.splitAt(seq.length / 2)
         if (seq.length > forkUntil)
            map2(fork(fold(l)(f, default)(forkUntil)), fork(fold(r)(f, default)(forkUntil)))(f)
         else
            map2(fold(l)(f, default)(forkUntil), fold(r)(f, default)(forkUntil))(f)
      }

   def max(ints: IndexedSeq[Int])(forkUntil: Integer): Par[Int] = fold(ints)(Math.max, 0)(forkUntil)

   def countWords(paragraphs: List[String]): Par[Int] = {

      val words = parMap(paragraphs)(x => x.split(" ").length)
      flatMap(words)(x => sum(x.toIndexedSeq))
   }

   def map3ViaFlatMap[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
      flatMap(a)(va => flatMap(b)(vb => map(c)(vc => f(va, vb, vc))))

   def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {

      val curriedFunction = f.curried

      val intermediate = map2(a, b)((a, b) => curriedFunction(a)(b))
      map2(intermediate, c)((function, c) => function(c))
   }

   def map3Compact[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = map2(map2(a, b)(f.curried(_)(_)), c)(_.apply(_))

   def map4Compact[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] =
      map2(map2(map2(a, b)(f.curried(_)(_)), c)(_.apply(_)), d)(_.apply(_))

   def map5Compact[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
      map2(map2(map2(map2(a, b)(f.curried(_)(_)), c)(_.apply(_)), d)(_.apply(_)), e)(_.apply(_))

   def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es =>
      {
         val index = run(es)(n).get
         choices(index)(es)
      }

   def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = choiceN(map(cond)(v => if (v) 0 else 1))(List(t, f))

   def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es =>
      {
         val computedKey = run(es)(key).get
         choices(computedKey)(es)
      }

   def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = flatMap(pa)(choices)

}
