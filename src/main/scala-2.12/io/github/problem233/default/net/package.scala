package io.github.problem233.default

import java.io.{BufferedReader, InputStreamReader}
import java.net.URL
import java.nio.charset.Charset

import scala.collection.JavaConverters._

/**
  * @author Problem233
  */
package object net {

  def read(url: URL, charsetName: String) = {
    val reader = new BufferedReader(new InputStreamReader(url.openStream(), Charset.forName(charsetName)))
    try reader.lines.iterator().asScala.reduceLeft(_ + "\n" + _) // uses SAM conversion
    finally reader.close()
  }

}
