package io.github.problem233.default.collection

import scala.language.implicitConversions

/**
  * @author Problem233
  */
package object stack {

  implicit def any2Stack[T](x: T): Stack[T] = new Stack(x :: Nil)

  implicit def stack2List[T](x: Stack[T]): List[T] = x.list

}
