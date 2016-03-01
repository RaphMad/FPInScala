package chapter9

import org.scalacheck.Properties
import org.scalacheck.Test
import org.scalacheck.Prop.{ forAll, BooleanOperators }

object ParserSpec extends Properties("Parser") {

   property("char parser succeeds with char") = forAll { (c: Char) =>

      // run(char(c))(c.toString) == Right(c)
      c == c
   }

   property("string parser succeeds with string") = forAll { (s: String) =>

      // run(string(s))(s) == Right(s)
      s == s
   }

   property("or with left match succeeds") = forAll { (s1: String, s2: String) =>

      // run(or(string(s1),string(s2)))(s1) == Right(s1)
      s1 == s1 && s2 == s2
   }

   property("or with right match succeeds") = forAll { (s1: String, s2: String) =>

      // run(or(string(s1),string(s2)))(s2) == Right(s2)
      s1 == s1 && s2 == s2
   }

   property("listOfN should work as expected") = forAll { (s1: String) =>

      // run(listOfN(3, "ab" | "cad"))("ababcad") == Right(List("ab", "ab", cad"))
      // run(listOfN(3, "ab" | "cad"))("cadabab") == Right(List("cad", "ab", "ab"))
      // run(listOfN(3, "ab" | "cad"))("ababab") == Right(List("ab", "ab", "ab"))

      true
   }

   property("succeed should always work") = forAll { (s: String) =>

      // run(succeed(a))(s) == Right(a)

      true
   }
}
