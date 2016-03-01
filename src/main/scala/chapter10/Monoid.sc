import chapter10.Monoid

val stringMonoid = new Monoid[String] {
  def op(a1: String, a2: String) = a1 + a2
  val zero = ""
}

stringMonoid.op("howdy", "ho")

def listMonoid[A] = new Monoid[List[A]] {
  def op(a1: List[A], a2: List[A]) = a1 ++ a2
  val zero = Nil
}

listMonoid.op(List(1, 2, 3), listMonoid.zero)

val intAddition = new Monoid[Int] {
  def op(a1: Int, a2: Int) = a1 + a2
  val zero = 0
}

intAddition.op(5, 3)

val intMultiplication = new Monoid[Int] {
  def op(a1: Int, a2: Int) = a1 * a2
  val zero = 1
}

intMultiplication.op(5, 3)

val booleanOr = new Monoid[Boolean] {
  def op(a1: Boolean, a2: Boolean) = a1 || a2
  val zero = false
}

booleanOr.op(true, false)

val booleanAnd = new Monoid[Boolean] {
  def op(a1: Boolean, a2: Boolean) = a1 && a2
  val zero = true
}

booleanAnd.op(true, false)

def optionMonoid[A] = new Monoid[Option[A]] {
  def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
  val zero = None
}

optionMonoid.op(optionMonoid.zero, Some(3))

def endoMonoid[A] = new Monoid[A => A] {
  def op(a1: A => A, a2: A => A) = a1 compose a2
  val zero = identity[A] _
}

val succ = (a: Int) => a + 1
val double = (a: Int) => a + a
endoMonoid.op(succ, double)(10)
