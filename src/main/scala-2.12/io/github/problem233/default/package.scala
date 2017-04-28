package io.github.problem233

/**
  * @author Problem233
  */
package object default {

  def timeTest[T](x: => T) = {
    val start = System.currentTimeMillis
    try x
    finally println(s"Elapsed time: ${System.currentTimeMillis - start}ms")
  }

  implicit class InComprehension[T](x: T) {
    def in[R](f: T => R) = f apply x
  }

}
