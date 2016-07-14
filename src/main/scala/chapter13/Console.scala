package chapter13

import chapter7.Nonblocking.Par

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
}

object Console {

  case class PrintLine(line: String) extends Console[Unit] {
    def toPar = Par.lazyUnit(println(line))
    def toThunk = () => println(line)
  }

  case object ReadLine extends Console[Option[String]] {
    def toPar = Par.lazyUnit(run)
    def toThunk = () => run
    def run: Option[String] =
      try Some(scala.io.StdIn.readLine())
      catch {
        case e: Exception => None
      }
  }
}
