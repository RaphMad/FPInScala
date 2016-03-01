package common

object TypeVsClass {

   // defining State as a type alias
   type StateType[S, A] = S => (A, S)

   // pro: concise usage
   def doSomething(statement: StateType[Int, Int]): Unit = {

      val (result, newState) = statement(5)
   }

   // con: functions have to be defined externally and need many type parameters
   def map[S, A, B](statement: StateType[S, A])(f: A => B): StateType[S, B] = s => {
      val (result, newState) = statement(s)
      (f(result), newState)
   }

   // pro: creating statements is straight-forward
   val statement: StateType[Int, Int] = x => (x, x)

   // con: cannot use '.' or infix notation
   map(statement)(x => x + 2)
   // statement.map(x => x + 2)
   // statement map (x => x + 2)

   // defining State as a class with a member 'run'

   // pro: functions can be defined internally, some type parameters and the 'this' parameter can be omitted
   class State[S, A](run: S => (A, S)) {

      def map[B](f: A => B): StateType[S, B] = s => {
         val (result, newState) = run(s)
         (f(result), newState)
      }
   }

   // con: creating statements has to use 'new State()'
   val statement2 = new State((x: Int) => (x, x))

   // pro: can use '.' and infix notation
   statement2.map(x => x + 2)
   statement2 map (x => x + 2)

   // pro/con: cannot 'run' from the outside
   def doSomething(statement: State[Int, Int]): Unit = {

      // val (result, newState) = statement.run(5)
   }
}
