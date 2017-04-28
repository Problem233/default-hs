import io.github.problem233.default.simulation.cppSimulation._
locally {
  import Console._
  print(
    s"""$GREEN==================================================================
       |${BOLD}Successfully loaded. Enjoy it!$RESET
       |${GREEN}Now, you can:
       |- Use the C++-style output
       |  eg: cout << "hello world" << endl;
       |  by: val cout = Console.out
       |      val endl = "\n"
       |      implicit class CppStyleOutput(ps: PrintStream) {
       |        def <<(x: Any): PrintStream
       |      }
       |==================================================================$RESET
     """.stripMargin)
}
