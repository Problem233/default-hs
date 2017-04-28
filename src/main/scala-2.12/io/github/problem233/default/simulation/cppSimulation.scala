package io.github.problem233.default.simulation

import java.io.PrintStream

/**
  * @author Problem233
  */
object cppSimulation {

  val cout = Console.out
  val endl = "\n"

  implicit class CppStyleOutput(ps: PrintStream) {
    def <<(x: Any) = try ps finally ps print x
  }

}
