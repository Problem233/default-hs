package io.github.problem233.default

import spire.implicits._
import spire.math._

/**
  * @author Problem233
  */
package object math {

  def coprime[T](a: T, b: T)(implicit integral: Integral[T]) =
    (integral gcd (a, b)) == 1

}
