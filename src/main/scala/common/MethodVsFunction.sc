package common

object MethodVsFunction {

  // blog post containing useful information on this topic: http://jim-mcbeath.blogspot.co.at/2009/05/scala-functions-vs-methods.html
  
  // 'def' defines ordinary methods - just like in java they 'belong' to their object
  def increment(x: Int) : Int = x + 1             //> increment: (x: Int)Int
  
  // 'val' defines values that are simply immutable 'fields' of their object
  val pi = 3.1415                                 //> pi  : Double = 3.1415
  
  // functions can be defined via the following construct (note that this is just special syntax for creating a new Function1[A, B] object)
  (x: Int) => x + 1                               //> res0: Int => Int = <function1>
  
  // since functions are values, they can be assigned to 'val'
  val inc = (x: Int) => x + 1                     //> inc  : Int => Int = <function1>
  
  // () invokes the function (note that this simply delegates to the 'apply()' method of the Function1[A, B] object)
  inc(5)                                          //> res1: Int = 6
  
  // no () represents the Function1[A, B] object 'itself'
  inc                                             //> res2: Int => Int = <function1>
  
  // methods are NOT values, the following line results in a compiler ERROR:
  // val error = increment
  
  // but methods can be converted to functions to become values:
  val m1 = x => increment(x)                      //> m1  : Int => Int = <function1>
  val m2 = increment(_)                           //> m2  : Int => Int = <function1>
  
  val m3 = increment _                            //> m3  : Int => Int = <function1>
  
  // the '_' in m3 is very different from the '_' in m2 - it converts ANY method to a function:
  
  def add(x: Int, y:Int) : Int = x + y            //> add: (x: Int, y: Int)Int
  val add3 = add _                                //> add3  : (Int, Int) => Int = <function2>
  
  // the 'trailing _' is indeed a special syntactic construct (also called 'Eta reduction'), the following line would be a compiler error:
  // val m4 = x => increment x
  
  object test {
  
  	def incMethod(x: Int) : Int = x + 1
  	val incFunction = incMethod _
  	
  	def fiveMethod() : Int = 5
  	val fiveFunc = fiveMethod _
  }
  
  // the rules for method invokation are a bit confusing, especially for 0-ary methods/function
  
  // invoke 1-ary function
  test.incFunction(5)                             //> res3: Int = 6
  
  // get 1-ary function 'itself'
  test.incFunction                                //> res4: Int => Int = <function1>
  
  // invoke 1-ary method
  test.incMethod(5)                               //> res5: Int = 6
  
  // ILLEGAL, methods cannot be 'passed around'
  // test.incMethod
  
  // to 'pass around' a method we have to convert it to a function:
  test.incMethod _                                //> res6: Int => Int = <function1>
  
  // invoke 0-ary function
  test.fiveFunc()                                 //> res7: Int = 5
  
  // get 0-ary function 'itself'
  test.fiveFunc                                   //> res8: () => Int = <function0>
  
  // invoke 0-ary method
  test.fiveMethod()                               //> res9: Int = 5
  
  // LEGAL, but INVOKES the 0-ary method:
  test.fiveMethod                                 //> res10: Int = 5
}