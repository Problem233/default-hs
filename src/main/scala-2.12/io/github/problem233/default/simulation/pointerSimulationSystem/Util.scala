package io.github.problem233.default.simulation.pointerSimulationSystem

/**
  * @author Problem233
  */
class Util(val pss: PointerSimulationSystem) {

  import pss._

  // Linked list utils

  case class LLNode[T](var value: T, var next: LLNodePointer[T])

  class LLNodePointer[T](address: Int) extends Pointer[LLNode[T]](address) {

    def nodeValue = value.value

    def nodeValue_=(newValue: T) = value.value = newValue

    def next = value.next

    override def toString = s"Node($nodeValue)@$address :: $next"

  }

  object LLNodePointer {

    def apply[T](value: T, next: LLNodePointer[T]) = {
      val address = (memory takeWhile (_ != null)).length
      memory(address) = LLNode(value, next)
      new LLNodePointer[T](address)
    }

  }

  def generateLL[T](length: Int) = {
    def generateLL(lengthLeft: Int, next: LLNodePointer[T]): LLNodePointer[T] =
      if (lengthLeft == 0) next
      else generateLL(lengthLeft - 1, LLNodePointer[T](null.asInstanceOf[T], next))
    generateLL(length - 1, LLNodePointer[T](null.asInstanceOf[T], null))
  }

}
