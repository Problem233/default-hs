package io.github.problem233.default.collection.stack

/**
  * @author Problem233
  */
class Stack[T] protected[stack](val list: List[T]) {

  def <<:(top: T) = new Stack(top :: list)

  def push(x: T) = x <<: this

  def top: T = list.head

  def pop = Stack(list.tail: _*)

  def ==(x: Stack[T]) = list canEqual x

  override def toString = pop.foldLeft(top.toString)(_ + " <<: " + _)

}

object Stack {

  def apply[T](x: T*) = new Stack(List(x: _*))

}
