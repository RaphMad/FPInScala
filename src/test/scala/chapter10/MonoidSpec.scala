package chapter10

import chapter10.Monoid._
import chapter8.Prop._
import chapter8._

object MonoidSpec {

  def assoc[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val tripleGen =
      gen flatMap (x =>
        gen flatMap (y =>
          gen map (z =>
            (x, y, z))))
    forAll(tripleGen) { case (x, y, z) => m.op(x, m.op(y, z)) == m.op(m.op(x, y), z) }
  }

  def id[A](m: Monoid[A], gen: Gen[A]): Prop = forAll(gen)((a: A) => m.op(a, m.zero) == a && m.op(m.zero, a) == a)

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = assoc(m, gen) && id(m, gen)

  def main(args: Array[String]): Unit = {
    stringMonoid_ShouldSatisfyLaws()

    listMonoid_ShouldSatisfyLaws()

    intAdditionMonoid_ShouldSatisfyLaws()
    intMultiplicationMonoid_ShouldSatisfyLaws()

    booleanOrMonoid_ShouldSatisfyLaws()
    booleanAndMonoid_ShouldSatisfyLaws()

    optionMonoid_ShouldSatisfyLaws()

    endoMonoid_ShouldSatisfyLaws()
  }

  def stringMonoid_ShouldSatisfyLaws(): Unit = {
    run(monoidLaws(stringMonoid, Gen.string.apply(10)))
  }

  def listMonoid_ShouldSatisfyLaws(): Unit = {
    val listGen = Gen.choose(1, 1000).listOf.apply(10)
    run(monoidLaws(listMonoid[Int], listGen))
  }

  def intAdditionMonoid_ShouldSatisfyLaws(): Unit = {
    run(monoidLaws(intAddition, Gen.choose(1, 1000)))
  }

  def intMultiplicationMonoid_ShouldSatisfyLaws(): Unit = {
    run(monoidLaws(intMultiplication, Gen.choose(1, 1000)))
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

  def endoMonoid_ShouldSatisfyLaws(): Unit = {
    // functions cannot be compared (they will reference different functions that actually _DO_ the same)
    // val functionGen = Gen.genEndoFn(Gen.choose(1, 1000))
    // run(monoidLaws(endoMonoid[Int], functionGen))

    // but we do some testing with concrete functions and feeding values
    val f1 = (a: Int) => a + 1
    val f2 = (a: Int) => a + a
    val f3 = (a: Int) => 10 * a
    val paramGen = Gen.choose(1, 1000)

    val assoc = forAll(Gen.choose(1, 1000)) { x =>
      endoMonoid.op(f1, endoMonoid.op(f2, f3))(x) == endoMonoid.op(endoMonoid.op(f1, f2), f3)(x)
    }

    val id = forAll(Gen.choose(1, 1000)) { x =>
      endoMonoid.op(f1, endoMonoid.zero)(x) == endoMonoid.op(endoMonoid.zero, f1)(x)
    }

    run(assoc && id)
  }
}
