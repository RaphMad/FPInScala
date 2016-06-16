package chapter13

import chapter13.IOTailRecFunctions._

object IOTailRecPrograms {

  def main(args: Array[String]): Unit = {
    //converterExpanded.run

    //IOTailRec.forever(PrintLine("Still going...")).run
  }

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def converter: IOTailRec[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def converterExpanded: IOTailRec[Unit] = {
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
