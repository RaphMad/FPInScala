package chapter8

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object ListSpecification extends Properties("List") {

   property("sum holds on reverse") = forAll { (l: List[Int]) =>
      l.sum == l.reverse.sum
   }

   // 8.1 b: if all elements in list are the same => list.sum == list.head * list.length

   property("sum equal to sum of halves") = forAll { (l: List[Int]) =>

      val (left, right) = l.splitAt(l.length / 2)

      l.sum == left.sum + right.sum
   }

   property("maximum >= all items") = forAll { (l: List[Int]) =>

      l.forall(x => l.max >= x)
   }
}
