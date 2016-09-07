package chapter13

import java.util.concurrent.Executors

import chapter13.IOTailRec.IOFunctions._
import chapter13.IOTailRec._
import chapter7.Nonblocking.Par

object Program {

  def main(args: Array[String]): Unit = {
    //converterExpanded.run
    //IO.forever(PrintLine("Still going...")).run

    /*val const42 = chapter13.Suspend(() => 42)
    val flatMap1 = const42.flatMap(v => chapter13.Return(3*v))
    val flatMap2 = flatMap1.flatMap(v => chapter13.Return(3*v))

    Free.runTrampoline(flatMap2)
    Free.run(flatMap2)(SampleMonads.function0Monad)()*/

    val f1: Free[Console, Option[String]] = for {
      _ <- Console.printLn("Enter value:")
      ln <- Console.readLn
      _ <- Console.printLn("Value was: " + ln)
      ln <- Console.readLn
    } yield ln

    //val result = Free.runConsoleFunction0(f1)()
    val par = Free.runConsolePar(f1)
    Par.run(Executors.newCachedThreadPool())(par)
  }

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def converterExpanded: IO[Unit] = {
    PrintLine("Enter a temperature in degrees Fahrenheit: ").flatMap(_ => {
      ReadLine.map(line => {
        line.toDouble
      }).flatMap(d => {
        PrintLine(fahrenheitToCelsius(d).toString)
      })
    })
  }

  val echo = ReadLine.flatMap(PrintLine)
  val readInt = ReadLine.map(_.toInt)
}
