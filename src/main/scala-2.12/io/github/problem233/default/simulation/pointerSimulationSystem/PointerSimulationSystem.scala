package io.github.problem233.default.simulation.pointerSimulationSystem

/**
  * @author Problem233
  */
class PointerSimulationSystem(memorySize: Int) {

  val memory = new Array[Any](memorySize)

  def memoryUsed = memory count (_ != null)

  def clear() = for (i <- 0 until memorySize) memory(i) = null

  class Pointer[T](address: Int) {

    def value = memory(address).asInstanceOf[T]

    def value_=(newValue: T) = memory(address) = newValue

    def dispose() = memory(address) = null

    override def toString = s"$value@$address"

  }

  object Pointer {

    def apply[T](value: T) = {
      val address = (memory takeWhile (_ != null)).length
      memory(address) = value
      new Pointer[T](address)
    }

  }

}
