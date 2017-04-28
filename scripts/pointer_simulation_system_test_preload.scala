import io.github.problem233.default.simulation.pointerSimulationSystem._
val pss = new PointerSimulationSystem(1024)
import pss._
val util = new Util(pss)
import util._
locally {
  import Console._
  print(
    s"""$GREEN==================================================================
       |${BOLD}Successfully loaded. Enjoy it!$RESET
       |${GREEN}Imported:
       |from `pss`:
       |  val memory: Array[Any]
       |  def memoryUsed: Int
       |  def clear: Unit
       |  class Pointer[T](address: Int) {
       |    def value: T
       |    def value_=(newValue: T): Unit
       |    def dispose: Unit
       |  }
       |  object Pointer {
       |    def apply[T](value: T): Pointer[T]
       |  }
       |from `util`:
       |  def generateLL[T](length: Int): LLNodePointer[T]
       |  case class LLNode[T](var value: T, var next: LLNodePointer[T])
       |  class LLNodePointer[T](address: Int) extends pss.Pointer[LLNode[T]](address) {
       |    def nodeValue: T
       |    def nodeValue_=(newValue: T): Unit
       |    def next: LLNodePointer[T]
       |  }
       |  object LLNodePointer {
       |    def apply[T](value: T, next: LLNodePointer[T]): LLNodePointer[T]
       |  }
       |==================================================================$RESET
     """.stripMargin)
}
