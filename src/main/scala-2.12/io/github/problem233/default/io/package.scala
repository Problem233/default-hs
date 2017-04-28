package io.github.problem233.default

import java.io._
import java.nio.charset.Charset

import scala.collection.JavaConverters._

/**
  * @author Problem233
  */
package object io {

  def forceNewFile(file: File) {
    if (file exists()) file.delete()
    file.mkdirs()
    file.delete()
    file.createNewFile()
  }

  def readText(file: File, charsetName: String) = {
    require(file.exists && file.isFile)
    val reader = new BufferedReader(new InputStreamReader(file.toURI.toURL.openStream(), Charset.forName(charsetName)))
    try reader.lines.iterator.asScala.reduceLeft(_ + "\n" + _)
    finally reader.close()
  }

  def writeText(file: File, text: String, charsetName: String) {
    if (file.exists()) require(file.isFile) else file.createNewFile()
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), Charset.forName(charsetName)))
    writer.write(text)
    writer.close()
  }

  def copy(from: File, to: File, force: Boolean) {
    import java.io.{FileInputStream, FileOutputStream}
    if (force) forceNewFile(to) else to.createNewFile()
    val fromStream = new FileInputStream(from)
    val toStream = new FileOutputStream(to)
    val inChannel = fromStream.getChannel
    val outChannel = toStream.getChannel
    inChannel.transferTo(0, inChannel.size(), outChannel)
    List(fromStream, toStream, inChannel, outChannel).foreach(_.close())
  }

}
