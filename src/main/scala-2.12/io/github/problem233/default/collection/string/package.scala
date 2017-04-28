package io.github.problem233.default.collection

import io.github.problem233.default._
import java.io._

/**
  * @author Problem233
  */
package object string {

  def fullPermutation(str: String): Set[String] =
    if (str.length == 1) Set(str)
    else (0 until str.length) map (i =>
      fullPermutation(str.substring(0, i) + str.substring(i + 1)) map (str(i) + _)
      ) in (_.toSet[Set[String]]) in (_.flatten)

  lazy val unicode = '\u0000' to '\uffff'

  def writeUnicodeFullTable (toFile: File) {
    val str = (unicode groupBy (_ / 16) map (_._2 mkString " ")).toList sortBy (_.head) map (str => ((str: String) => "U+" + "0" * (4 - str.length) + str)(str.head.toHexString) + " " + str) mkString "\n"
    val writer = new FileWriter (toFile)
    try writer write str finally writer.close()
  }

}
