package chapter10

import java.util.concurrent.Executors

import chapter7.Nonblocking.Par
import chapter8.Prop._
import chapter8._
import SampleMonoids._

object MonoidSpec {

  def assoc[A](m: Monoid[A], gen: Gen[A]): Prop = {
    /*val tripleGen =
      gen flatMap (x =>
        gen flatMap (y =>
          gen map (z =>
            (x, y, z))))*/

    val tripleGen = for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z)

    forAll(tripleGen) { case (x, y, z) => m.op(x, m.op(y, z)) == m.op(m.op(x, y), z) }
  }

  def id[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen)((a: A) => m.op(a, m.zero) == a && m.op(m.zero, a) == a)

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    assoc(m, gen) && id(m, gen)

  def main(args: Array[String]): Unit = {
    /*stringMonoid_ShouldSatisfyLaws()

    listMonoid_ShouldSatisfyLaws()

    intAdditionMonoid_ShouldSatisfyLaws()
    intMultiplicationMonoid_ShouldSatisfyLaws()

    booleanOrMonoid_ShouldSatisfyLaws()
    booleanAndMonoid_ShouldSatisfyLaws()

    optionMonoid_ShouldSatisfyLaws()

    endoMonoid_ShouldSatisfyLaws()

    parMap_ShouldWorkInParallel()*/

    wcMonoid_ShouldSatisfyLaws()

    /*simpleOrderMonoid_ShouldSatisfyLaws()
    simpleOrderMonoid_ShouldSatisfyIdLaws()
    simpleOrderMonoid_ShouldSatisfyAssocLaws()*/
    //exampleOrderMonoid_ShouldSatisfyLaws()

    //productMonoid_ShouldSatisfyLaws()
  }

  def stringMonoid_ShouldSatisfyLaws(): Unit = {
    run(monoidLaws(stringMonoid, Gen.string.apply(10)))
  }

  def listMonoid_ShouldSatisfyLaws(): Unit = {
    val listGen = Gen.choose(1, 1000).listOf.apply(10)
    run(monoidLaws(listMonoid[Int], listGen))
  }

  def intAdditionMonoid_ShouldSatisfyLaws(): Unit = {
    run(monoidLaws(intAddition, Gen.choose(-1000, 1000)))
  }

  def intMultiplicationMonoid_ShouldSatisfyLaws(): Unit = {
    run(monoidLaws(intMultiplication, Gen.choose(-1000, 1000)))
  }

  def booleanOrMonoid_ShouldSatisfyLaws(): Unit = {
    run(monoidLaws(booleanOr, Gen.boolean))
  }

  def booleanAndMonoid_ShouldSatisfyLaws(): Unit = {
    run(monoidLaws(booleanAnd, Gen.boolean))
  }

  def optionMonoid_ShouldSatisfyLaws(): Unit = {
    val justIntGen = Gen.choose(1, 1000).map(i => Some(i))
    val optionGen = Gen.weighted((justIntGen, 50), (Gen.unit(None), 50))
    run(monoidLaws(optionMonoid[Int], optionGen))
  }

  def exampleOrderMonoid_ShouldSatisfyLaws(): Unit = {
    val tripleGen = for {
      x <- Gen.choose(1, 1000)
      y <- Gen.choose(1, 1000)
      z <- Gen.boolean
    } yield Some(x, y, z)

    val gen = Gen.weighted((tripleGen, 80), (Gen.unit(None), 20))
    run(monoidLaws(exampleOrderMonoid, gen))
  }

  def simpleOrderMonoid_ShouldSatisfyLaws(): Unit = {
    val tripleGen = for {
      x <- Gen.choose(1, 1000)
      y <- Gen.boolean
    } yield Some(x, y)

    val gen = Gen.weighted((tripleGen, 80), (Gen.unit(None), 20))
    run(monoidLaws(simpleOrderMonoid, gen))
  }

  def simpleOrderMonoid_ShouldSatisfyIdLaws(): Unit = {
    val tripleGen = for {
      x <- Gen.choose(1, 1000)
      y <- Gen.boolean
    } yield Some(x, y)

    val gen = Gen.weighted((tripleGen, 80), (Gen.unit(None), 20))
    run(id(simpleOrderMonoid, gen))
  }

  def simpleOrderMonoid_ShouldSatisfyAssocLaws(): Unit = {
    val tripleGen = for {
      x <- Gen.choose(1, 1000)
      y <- Gen.boolean
    } yield Some(x, y)

    val gen = Gen.weighted((tripleGen, 80), (Gen.unit(None), 20))
    run(assoc(simpleOrderMonoid, gen))
  }

  def endoMonoid_ShouldSatisfyLaws(): Unit = {
    // functions cannot be compared (they will _DO_ the same but still be different references)
    // val functionGen = Gen.genConstFn(Gen.choose(1, 1000))
    // run(monoidLaws(endoMonoid[Int], functionGen))

    // but we can do some (non-conclusive) testing by feeding values to concrete functions
    val f1 = (a: Int) => a + 1
    val f2 = (a: Int) => a + a
    val f3 = (a: Int) => 10 * a

    val assoc = forAll(Gen.choose(1, 1000)) { x =>
      endoMonoid.op(f1, endoMonoid.op(f2, f3))(x) == endoMonoid.op(endoMonoid.op(f1, f2), f3)(x)
    }

    val id = forAll(Gen.choose(1, 1000)) { x =>
      endoMonoid.op(f1, endoMonoid.zero)(x) == endoMonoid.op(endoMonoid.zero, f1)(x)
    }

    run(assoc && id)
  }

  def parMap_ShouldWorkInParallel(): Unit = {
    val list = IndexedSeq.fill(100)("2")

    // val result = Monoid.parFoldMap4(list, SampleMonoids.intMultiplication)(Integer.parseInt)
    val result = Monoid.parFoldMap6(list, SampleMonoids.intMultiplication)(Integer.parseInt)
    println("Result is: " + Par.run(Executors.newCachedThreadPool())(result))
  }

  def wcMonoid_ShouldSatisfyLaws(): Unit = {

    val noSpaceString = Gen.stringNoSpaceN(10)
    val emptyStringGen = Gen.unit("").unsized.apply(5)

    val stubGen = Gen.weighted((emptyStringGen, 20), (noSpaceString, 80)) map (s => Stub(s))

    val partStringGen = Gen.weighted((emptyStringGen, 50), (noSpaceString, 50))
    val partGen = for {
      l <- partStringGen
      n <- Gen.smallInt
      r <- partStringGen
    } yield Part(l, n, r)

    val wcGen = Gen.weighted((stubGen, 50), (partGen, 50))
    run(monoidLaws(wcMonoid, wcGen))
  }

  def productMonoid_ShouldSatisfyLaws(): Unit = {
    val productMonoid = Monoid.productMonoid(intAddition, booleanAnd);
    val gen = for {
      x <- Gen.choose(-1000, 1000)
      y <- Gen.boolean
    } yield (x, y)

    run(monoidLaws(productMonoid, gen))
  }
}
