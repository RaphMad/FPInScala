package chapter9

trait Parser[A]
trait ParseError

trait Parsers[ParseError, Parser[+_]] { self =>

   def run[A](p: Parser[A])(input: String): Either[ParseError, A]

   implicit def char(c: Char): Parser[Char] =
      string(c.toString) map (_.charAt(0))

   implicit def string(s: String): Parser[String]

   def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

   def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
   def many[A](p: Parser[A]): Parser[List[A]]
   def many1[A](p: Parser[A]): Parser[List[A]]

   def map[A, B](a: Parser[A])(f: A => B): Parser[B]
   def succeed[A](a: A): Parser[A] = string("") map (_ => a)

   def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)]

   def slice[A](p: Parser[A]): Parser[String]

   // implicit conversion for strings
   implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

   // introduces infix syntax "a | b" and "a or b"
   implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

   case class ParserOps[A](p: Parser[A]) {

      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

      def many: Parser[List[A]] = self.many(p)
      def many1: Parser[List[A]] = self.many1(p)

      def map[B](f: A => B): Parser[B] = self.map(p)(f)

      def **[B >: A](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
      def product[B >: A](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
   }
}
