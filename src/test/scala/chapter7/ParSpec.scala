package chapter7

import java.util.concurrent._
import java.util.Random
import org.scalatest._
import org.scalatest.concurrent._
import org.scalatest.time.SpanSugar._

import chapter7.par._

class ParSpec extends FlatSpec with Matchers with BeforeAndAfter with Timeouts {

   "A parallel computation" should "yield the same result as an ordinary one" in {

      val result = 1 + 2

      val parComputation = map2(unit(1), unit(2))(_ + _)
      val parResult = runWithThreadPool(parComputation)

      parResult should equal(result)
   }

   it should "yield the same result when forking internally" in {

      val parComputation = map2(unit(1), unit(2))(_ + _)
      val parResult = runWithThreadPool(parComputation)

      val forkedComputation = map2(lazyUnit(1), lazyUnit(2))(_ + _)
      val forkedResult = runWithThreadPool(forkedComputation)

      forkedResult should equal(parResult)
   }

   it should "not block when fork is nested using a CachedThreadPool" in {

      // exercise 7.8
      // exercise 7.9: use more nested forks than available threads
      val nestedComputation = fork(fork(unit(1)))

      failAfter(1 second) {
         val result = runWithThreadPool(nestedComputation)

         result should be(1)
      }
   }

   it should "not block when fork is nested using a SingleThreadExecutor" in {

      val nestedComputation = fork(fork(unit(1)))

      failAfter(1 second) {
         val result = runWithSingleThread(nestedComputation)

         result should be(1)
      }
   }

   it should "perform async functions the same way as normal functions" in {

      val result = numberCruncher(42)

      val parComputation = asyncF(numberCruncher)(42)
      val parResult = runWithThreadPool(parComputation)

      result should equal(parResult)
   }

   it should "map as expected" in {

      val list = List.range(1, 1000)

      val result = list map (x => x * 2)

      val parComputation = parMap(list)(x => x * 2)
      val parResult = runWithThreadPool(parComputation)

      result should equal(parResult)
   }

   it should "map fast" in {

      val list = List.range(1, 100)

      val parComputation = parMap(list)(numberCruncher)

      // a serial map would need about 25s (~250ms per item)
      failAfter(5 seconds) {
         runWithThreadPool(parComputation)
      }
   }

   it should "filter as expected" in {

      val list = List.range(1, 1000)

      val result = list filter (x => x % 2 == 0)

      val parComputation = parFilter(list)(x => x % 2 == 0)
      val parResult = runWithThreadPool(parComputation)

      result should equal(parResult)
   }

   it should "filter fast" in {

      val list = List.range(1, 100)

      val parComputation = parFilter(list)(slowFilterForEven)

      // a serial filter would need about 25s (~250ms per item)
      failAfter(5 seconds) {
         runWithThreadPool(parComputation)
      }
   }

   it should "find the maximum fast" in {

      val list = List.range(1, 10000000).toIndexedSeq

      val parComputation = max(list)(1000000)

      // a serial filter would need about 25s (~250ms per item)
      failAfter(20 seconds) {
         val time = System.nanoTime()
         runWithThreadPool(parComputation)
         println("Time elapsed: " + TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - time))
      }
   }

   it should "find the maximum fast serial" in {

      val list = List.range(1, 10000000).toIndexedSeq

      val time = System.nanoTime()
      list.max

      println("Time elapsed: " + TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - time))
   }

   it should "count words correctly" in {

      val text = List("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer id cursus quam, eget sodales elit. Donec nec felis fringilla velit finibus tincidunt. Curabitur mattis orci ut mauris fermentum cursus. Proin non ex lacinia, bibendum risus non, consequat arcu. Quisque imperdiet convallis dui. Morbi auctor luctus orci, et fringilla turpis placerat vel. Nullam vel viverra orci, a porta orci.",
         "Fusce dignissim eros a nulla convallis lacinia. Nulla sit amet tempus neque. Mauris vehicula elit sed est egestas, ut interdum ex dictum. Aliquam ut bibendum felis. Nulla fringilla, dolor at pellentesque tincidunt, lectus sapien facilisis est, in ullamcorper lacus turpis sed nisl. Sed quam sapien, posuere eget consequat ac, pulvinar quis tortor. Donec risus purus, tempus at condimentum ut, laoreet eu orci. Vestibulum ac nisl non leo molestie cursus ac id urna. Pellentesque placerat id nulla sit amet aliquet. Maecenas viverra augue felis, sit amet molestie sapien venenatis a. Integer facilisis tellus vitae diam posuere, aliquam ornare ligula malesuada. Nam eget ipsum sit amet quam commodo venenatis. Duis urna elit, laoreet ut consectetur id, blandit quis felis.",
         "Vestibulum ac pellentesque nibh. Donec vehicula leo nec faucibus mattis. In hac habitasse platea dictumst. Donec auctor, sapien ut scelerisque viverra, lectus nisl commodo dui, vitae posuere libero arcu a nulla. Vivamus tincidunt lorem eget leo porttitor, sit amet sagittis justo iaculis. Phasellus eget sem tempor, auctor odio interdum, molestie purus. Suspendisse sit amet pellentesque nibh. Curabitur feugiat sed augue a vehicula. Vivamus laoreet, lorem at maximus euismod, mi orci mollis enim, ac faucibus tortor dui ut ipsum. In hac habitasse platea dictumst. Fusce aliquet a nulla nec tempus. Curabitur ac eros quis tortor tincidunt faucibus. Sed ligula ipsum, tempus ac mi id, ultrices efficitur velit. Aliquam erat volutpat.",
         "Donec facilisis, velit vel consectetur dignissim, quam orci egestas turpis, et efficitur ipsum lectus sed lorem. Cras ornare lacus vitae enim pharetra molestie. Etiam placerat efficitur eros in rhoncus. Curabitur id magna at augue molestie pulvinar. Praesent aliquet turpis non vestibulum fringilla. Proin pharetra tortor mattis enim venenatis, et tincidunt purus suscipit. Vestibulum quis malesuada ex. Nunc turpis turpis, tristique non mattis id, tristique at nunc. Quisque quis elit nisl. Aliquam ut magna a lacus finibus tincidunt. Donec venenatis nec ante sit amet euismod. Phasellus facilisis sit amet nunc quis imperdiet. Ut sit amet dui turpis. Vivamus ac dui ut sem pharetra lacinia sed id justo. Etiam placerat quam turpis. Nunc sed mi ex.",
         "Nam id lorem a eros rhoncus varius sit amet mollis tortor. Mauris vitae tortor elementum, egestas risus quis, euismod justo. Vestibulum id velit arcu. Morbi efficitur felis ut erat sollicitudin, non elementum metus imperdiet. Nullam massa magna, mollis id sagittis at, ultricies sit amet ante. Cras bibendum nec purus tincidunt malesuada. Donec odio velit, rhoncus non dolor vel, egestas vulputate dui. Sed at auctor diam. Fusce in orci est. Nunc porta molestie elit, eget efficitur justo blandit id.")

      val computation = countWords(text)
      val result = runWithThreadPool(computation)

      result should equal(476)
   }

   def runWithThreadPool[A](c: Par[A]): A = par.run(Executors.newCachedThreadPool())(c).get()
   def runWithSingleThread[A](c: Par[A]): A = par.run(Executors.newSingleThreadExecutor())(c).get()

   def numberCruncher(x: Int): Int = {

      // vary calculation times between 0 and 500ms
      Thread.sleep(new Random().nextInt(500))

      // hard calculation ;)
      x + 1
   }

   def slowFilterForEven(x: Int): Boolean = {

      // vary filter times between 0 and 500ms
      Thread.sleep(new Random().nextInt(500))

      // hard calculation ;)
      x % 2 == 0
   }
}
